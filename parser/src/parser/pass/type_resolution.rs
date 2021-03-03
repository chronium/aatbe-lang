use crate::ast::{AtomKind, Expression, FunctionType, LValue, PrimitiveType, TypeKind, AST};

pub fn type_resolution(variants: &Vec<String>, ast: &AST) -> AST {
    match ast {
        AST::File(ast) => AST::File(
            ast.iter()
                .map(|ast| type_resolution(variants, ast))
                .collect(),
        ),
        AST::Expr(expr) => AST::Expr(resolve_expr(variants, expr)),
        AST::Typedef {
            name,
            type_names,
            variants: vars,
        } => AST::Typedef {
            name: name.clone(),
            type_names: type_names.clone(),
            variants: vars.as_ref().map(|vars| {
                vars.iter()
                    .map(|variant| resolve_typekind(variants, variant))
                    .collect()
            }),
        },
        AST::Import(_) => ast.clone(),
        AST::Record(name, types, fields) => AST::Record(
            name.clone(),
            types.clone(),
            fields.iter().map(|f| resolve_prim(variants, f)).collect(),
        ),
        AST::Module(_, _) => ast.clone(),
        _ => panic!("unhandled {:?}", ast),
    }
}

fn resolve_typekind(variants: &Vec<String>, ty: &TypeKind) -> TypeKind {
    match ty {
        TypeKind::Newtype(prim) => TypeKind::Newtype(resolve_prim(variants, prim)),
        TypeKind::Variant(name, ty) => TypeKind::Variant(
            name.clone(),
            ty.as_ref()
                .map(|tys| tys.iter().map(|ty| resolve_prim(variants, ty)).collect()),
        ),
    }
}

fn resolve_expr(variants: &Vec<String>, ast: &Expression) -> Expression {
    match ast {
        Expression::Function {
            attributes,
            type_names,
            name,
            body,
            ty,
            export,
        } => Expression::Function {
            name: name.clone(),
            attributes: attributes.clone(),
            type_names: type_names.clone(),
            export: *export,
            ty: resolve_function_ty(variants, ty),
            body: body.as_ref().map(|body| box resolve_expr(variants, &body)),
        },
        Expression::Block(exprs) => Expression::Block(
            exprs
                .iter()
                .map(|expr| resolve_expr(variants, expr))
                .collect(),
        ),
        Expression::Decl {
            ty,
            value,
            exterior_bind,
        } => Expression::Decl {
            ty: resolve_prim(variants, ty),
            value: value.as_ref().map(|val| box resolve_expr(variants, &val)),
            exterior_bind: exterior_bind.clone(),
        },
        Expression::Call { name, types, args } => Expression::Call {
            name: name.clone(),
            types: types.iter().map(|ty| resolve_prim(variants, ty)).collect(),
            args: args.iter().map(|arg| resolve_expr(variants, arg)).collect(),
        },
        Expression::Atom(atom) => Expression::Atom(resolve_atomkind(variants, atom)),
        Expression::Binary(box lhs, op, box rhs) => Expression::Binary(
            box resolve_expr(variants, lhs),
            op.clone(),
            box resolve_expr(variants, rhs),
        ),
        Expression::Assign {
            lval,
            value: box value,
        } => Expression::Assign {
            lval: resolve_lval(variants, lval),
            value: box resolve_expr(variants, value),
        },
        Expression::If {
            is_expr,
            cond_expr: box cond_expr,
            else_expr,
            then_expr: box then_expr,
        } => Expression::If {
            is_expr: *is_expr,
            cond_expr: box resolve_expr(variants, cond_expr),
            else_expr: else_expr
                .as_ref()
                .map(|box ex| box resolve_expr(variants, &ex)),
            then_expr: box resolve_expr(variants, then_expr),
        },
        Expression::RecordInit {
            record,
            types,
            values,
        } => Expression::RecordInit {
            record: record.clone(),
            types: types.iter().map(|ty| resolve_prim(variants, ty)).collect(),
            values: values
                .iter()
                .map(|val| resolve_atomkind(variants, val))
                .collect(),
        },
        _ => panic!("unhandled {:?}", ast),
    }
}

fn resolve_lval(variants: &Vec<String>, lval: &LValue) -> LValue {
    match lval {
        LValue::Index(lval, box expr) => {
            LValue::Index(lval.clone(), box resolve_expr(variants, expr))
        }
        _ => lval.clone(),
    }
}

fn resolve_atomkind(variants: &Vec<String>, atom: &AtomKind) -> AtomKind {
    match atom {
        AtomKind::Bool(_)
        | AtomKind::Unit
        | AtomKind::StringLiteral(_)
        | AtomKind::CharLiteral(_)
        | AtomKind::Ident(_)
        | AtomKind::Access(_) => atom.clone(),
        AtomKind::Integer(v, ty) => AtomKind::Integer(*v, resolve_prim(variants, ty)),
        AtomKind::Floating(v, ty) => AtomKind::Floating(*v, resolve_prim(variants, ty)),
        AtomKind::Unary(op, box atom) => {
            AtomKind::Unary(op.clone(), box resolve_atomkind(variants, atom))
        }
        AtomKind::Parenthesized(box expr) => {
            AtomKind::Parenthesized(box resolve_expr(variants, expr))
        }
        AtomKind::NamedValue { name, val: box val } => AtomKind::NamedValue {
            name: name.clone(),
            val: box resolve_expr(variants, val),
        },
        AtomKind::Cast(box atom, ty) => AtomKind::Cast(
            box resolve_atomkind(variants, &atom),
            resolve_prim(variants, &ty),
        ),
        AtomKind::Index(box atom, box expr) => AtomKind::Index(
            box resolve_atomkind(variants, &atom),
            box resolve_expr(variants, &expr),
        ),
        AtomKind::Ref(box atom) => AtomKind::Ref(box resolve_atomkind(variants, &atom)),
        AtomKind::SymbolLiteral(s) => AtomKind::SymbolLiteral(s.clone()),
        AtomKind::Array(expr) => {
            AtomKind::Array(expr.iter().map(|e| resolve_expr(variants, &e)).collect())
        }
        _ => panic!("unhandled {:?}", atom),
    }
}

fn resolve_function_ty(variants: &Vec<String>, ty: &FunctionType) -> FunctionType {
    FunctionType {
        ext: ty.ext,
        ret_ty: box resolve_prim(variants, &ty.ret_ty),
        params: ty
            .params
            .iter()
            .map(|param| resolve_prim(variants, param))
            .collect(),
    }
}

fn resolve_prim(variants: &Vec<String>, ty: &PrimitiveType) -> PrimitiveType {
    match ty {
        PrimitiveType::TypeRef(ty) => {
            if variants.contains(ty) {
                PrimitiveType::VariantType(ty.clone())
            } else {
                PrimitiveType::TypeRef(ty.clone())
            }
        }
        PrimitiveType::NamedType { name, ty } => PrimitiveType::NamedType {
            name: name.clone(),
            ty: ty.as_ref().map(|ty| box resolve_prim(variants, &ty)),
        },
        _ => ty.clone(),
    }
}
