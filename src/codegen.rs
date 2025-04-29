use crate::ast::{Expr, ExprKind, Statement, StatementKind};
use crate::lex::Operator;
use qbe::{Cmp, Function, Instr, Linkage, Module, Type, Value};
use std::collections::HashMap;

pub fn generate_ir(ast: &[Statement]) -> String {
    let mut module = Module::new();
    let mut main_func = Function::new(Linkage::public(), "main", vec![], Some(Type::Word));

    let mut temp_count = 0;

    let mut variables: HashMap<String, Value> = HashMap::new();
    main_func.add_block("start");

    for st in ast {
        generate_statement(&mut main_func, st, &mut temp_count, &mut variables);
    }

    module.add_function(main_func).to_string()
}

fn generate_statement(
    func: &mut Function,
    st: &Statement,
    temp_count: &mut usize,
    vars: &mut HashMap<String, Value>,
) {
    match st {
        Statement {
            kind: StatementKind::Return(expr),
            ..
        } => {
            if let ExprKind::Number(v) = expr.kind {
                func.add_instr(Instr::Ret(Some(Value::Const(v))));
            } else {
                let result = eval_expr(func, expr, temp_count, vars);
                func.add_instr(Instr::Ret(Some(result)));
            }
        }
        Statement {
            kind: StatementKind::DefineVar { name, value },
            ..
        } => {
            let value = eval_expr(func, value, temp_count, vars);
            vars.insert((*name).to_string(), value);
        }
        Statement {
            kind: StatementKind::AssignVar { name, value },
            ..
        } => {
            let value = eval_expr(func, value, temp_count, vars);
            *vars.get_mut(&(*name).to_string()).unwrap() = value;
        }
        Statement {
            kind: StatementKind::If { cond, block },
            ..
        } => {
            let cond = eval_expr(func, cond, temp_count, vars);

            let condition = Value::Temporary(format!("condition_{temp_count}"));

            let then_label_name = format!("then_{temp_count}");
            let merge_label_name = format!("merge_{temp_count}");

            func.assign_instr(
                condition.clone(),
                Type::Word,
                Instr::Cmp(Type::Word, Cmp::Ne, cond, Value::Const(0)),
            );

            func.add_instr(Instr::Jnz(
                condition,
                then_label_name.clone(),
                merge_label_name.clone(),
            ));

            func.add_block(then_label_name);
            for st in block {
                generate_statement(func, st, temp_count, vars);
            }
            func.add_block(merge_label_name);
        }
        Statement {
            kind: StatementKind::While { cond, block },
            ..
        } => {
            let cond_label = format!("for_cond_{temp_count}");
            let body_label = format!("for_body_{temp_count}");
            let exit_label = format!("for_exit_{temp_count}");

            func.add_instr(Instr::Jmp(cond_label.clone()));

            func.add_block(cond_label.clone());
            let cond_val = eval_expr(func, cond, temp_count, vars);
            let cmp_temp = Value::Temporary(format!("t_cond_{temp_count}"));
            func.assign_instr(
                cmp_temp.clone(),
                Type::Word,
                Instr::Cmp(Type::Word, Cmp::Ne, cond_val, Value::Const(0)),
            );
            func.add_instr(Instr::Jnz(cmp_temp, body_label.clone(), exit_label.clone()));

            func.add_block(body_label.clone());
            for stmt in block {
                generate_statement(func, stmt, temp_count, vars);
            }
            func.add_instr(Instr::Jmp(cond_label.clone()));

            func.add_block(exit_label.clone());
        }

        _ => todo!("Handle other statement types"),
    }
}

fn eval_expr(
    func: &mut Function,
    expr: &Expr,
    idx: &mut usize,
    vars: &HashMap<String, Value>,
) -> Value {
    match &expr.kind {
        ExprKind::Number(v) => Value::Const(*v),
        ExprKind::VariableName(name) => vars.get(&name.to_string()).unwrap().clone(),
        ExprKind::Binary { left, op, right } => {
            let left = eval_expr(func, left, idx, vars);
            let right = eval_expr(func, right, idx, vars);

            *idx += 1;
            let result_temporary = Value::Temporary(format!("t{idx}"));

            let operation_instruction = match op {
                Operator::Plus => Instr::Add(left, right),
                Operator::Minus => Instr::Sub(left, right),
                Operator::Star => Instr::Mul(left, right),
                Operator::Slash => Instr::Div(left, right),
                Operator::More => Instr::Cmp(Type::Word, Cmp::Sgt, left, right),
                Operator::Less => Instr::Cmp(Type::Word, Cmp::Slt, left, right),
                _ => unreachable!(),
            };
            func.assign_instr(
                Value::Temporary(format!("t{idx}")),
                Type::Word,
                operation_instruction,
            );
            result_temporary
        }
    }
}
