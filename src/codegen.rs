use crate::ast::{Expr, ExprKind, Statement, StatementKind};
use crate::lex::Operator;
use qbe::{Cmp, Function, Instr, Linkage, Module, Type, Value};
use std::collections::HashMap;

pub fn generate_ir(ast: &[Statement]) -> String {
    let mut module = Module::new();
    let mut main_func = Function::new(Linkage::public(), "main", vec![], Some(Type::Word));

    let mut temp_count = 0;
    let mut variables: HashMap<&str, Value> = HashMap::new();
    main_func.add_block("start");

    for st in ast {
        generate_statement(&mut main_func, st, &mut temp_count, &mut variables);
    }

    module.add_function(main_func).to_string()
}

fn generate_statement<'a>(
    func: &mut Function,
    st: &'a Statement,
    temp_count: &mut usize,
    vars: &mut HashMap<&'a str, Value>,
) {
    match st {
        Statement {
            kind: StatementKind::Return(expr),
            ..
        } => {
            let ret_val = match &expr.kind {
                ExprKind::Number(v) => Value::Const(*v),
                _ => eval_expr(func, expr, temp_count, vars),
            };
            func.add_instr(Instr::Ret(Some(ret_val)));
        }
        Statement {
            kind: StatementKind::DefineVar { name, value },
            ..
        } => {
            let ptr = Value::Temporary(format!("ptr_{name}"));
            func.assign_instr(ptr.clone(), Type::Long, Instr::Alloc8(1));
            let init_val = eval_expr(func, value, temp_count, vars);
            func.add_instr(Instr::Store(Type::Word, ptr.clone(), init_val));
            vars.insert(name, ptr);
        }
        Statement {
            kind: StatementKind::AssignVar { name, value },
            ..
        } => {
            let ptr = vars.get(name).expect("Undefined variable").clone();
            let new_val = eval_expr(func, value, temp_count, vars);
            func.add_instr(Instr::Store(Type::Word, ptr, new_val));
        }
        Statement {
            kind: StatementKind::If { cond, block },
            ..
        } => {
            let cond_val = eval_expr(func, cond, temp_count, vars);
            let cond_temp = Value::Temporary(format!("c{temp_count}"));
            func.assign_instr(
                cond_temp.clone(),
                Type::Word,
                Instr::Cmp(Type::Word, Cmp::Ne, cond_val, Value::Const(0)),
            );

            let then_lbl = format!("then_{temp_count}");
            let merge_lbl = format!("merge_{temp_count}");
            func.add_instr(Instr::Jnz(cond_temp, then_lbl.clone(), merge_lbl.clone()));

            func.add_block(then_lbl.clone());
            for st in block {
                generate_statement(func, st, temp_count, vars);
            }
            func.add_block(merge_lbl);
        }
        Statement {
            kind: StatementKind::While { cond, block },
            ..
        } => {
            *temp_count += 1;
            let id = *temp_count;
            let hdr = format!("while_header_{id}");
            let body = format!("while_body_{id}");
            let exit = format!("while_exit_{id}");

            func.add_instr(Instr::Jmp(hdr.clone()));
            func.add_block(hdr.clone());

            let cond_val = eval_expr(func, cond, temp_count, vars);
            let cmp_tmp = Value::Temporary(format!("c{id}_"));
            func.assign_instr(
                cmp_tmp.clone(),
                Type::Word,
                Instr::Cmp(Type::Word, Cmp::Ne, cond_val, Value::Const(0)),
            );
            func.add_instr(Instr::Jnz(cmp_tmp, body.clone(), exit.clone()));

            func.add_block(body.clone());
            for st in block {
                generate_statement(func, st, temp_count, vars);
            }
            func.add_instr(Instr::Jmp(hdr));
            func.add_block(exit);
        }
        _ => todo!("Handle other statements"),
    }
}

fn eval_expr(
    func: &mut Function,
    expr: &Expr,
    idx: &mut usize,
    vars: &HashMap<&str, Value>,
) -> Value {
    match &expr.kind {
        ExprKind::Number(v) => Value::Const(*v),
        ExprKind::VariableName(name) => {
            let ptr = vars.get(name).expect("Undefined variable").clone();
            *idx += 1;
            let tmp = Value::Temporary(format!("t{idx}"));
            func.assign_instr(tmp.clone(), Type::Word, Instr::Load(Type::Word, ptr));
            tmp
        }
        ExprKind::Binary { left, op, right } => {
            let l = eval_expr(func, left, idx, vars);
            let r = eval_expr(func, right, idx, vars);
            *idx += 1;
            let res = Value::Temporary(format!("t{idx}"));
            let instr = match op {
                Operator::Plus => Instr::Add(l, r),
                Operator::Minus => Instr::Sub(l, r),
                Operator::Star => Instr::Mul(l, r),
                Operator::Slash => Instr::Div(l, r),
                Operator::More => Instr::Cmp(Type::Word, Cmp::Sgt, l, r),
                Operator::Less => Instr::Cmp(Type::Word, Cmp::Slt, l, r),
                _ => unreachable!(),
            };
            func.assign_instr(res.clone(), Type::Word, instr);
            res
        }
    }
}
