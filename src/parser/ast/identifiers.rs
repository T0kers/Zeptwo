use super::*;
use crate::position::{Pos, Position};
use std::collections::HashMap;

macro_rules! newtype_index {
    ($name:ident, $index_type:ty) => {
        #[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
        pub struct $name(usize);

        impl $name {
            fn new(id: usize) -> Self {
                Self(id)
            }
        }
        impl $name {
            fn next_id(vec: &[$index_type]) -> $name {
                $name::new(vec.len())
            }
        }
        impl std::ops::Index<$name> for Vec<$index_type> {
            type Output = $index_type;

            fn index(&self, index: $name) -> &Self::Output {
                &self[index.0]
            }
        }
        impl std::ops::IndexMut<$name> for Vec<$index_type> {
            fn index_mut(&mut self, index: $name) -> &mut Self::Output {
                &mut self[index.0]
            }
        }
    };
}

pub struct IdentifierLookup {
    pub variable_info_vec: Vec<VariableInfo>,
    pub ambiguous_function_vec: Vec<Vec<FunctionID>>,
    pub ambiguous_function_map: HashMap<&'static str, AmbiguousFunctionID>,
    pub function_id_vec: Vec<FunctionInfo>,
    pub global_variable_definitions: Vec<Stmt>,
    pub scope_identifier_map: Vec<HashMap<&'static str, IdentifierID>>,
}

impl IdentifierLookup {
    pub fn check_types(&mut self) -> Result<(), ZeptwoError> {
        let mut dependencies = vec![];
        for global in &mut self.global_variable_definitions.clone() {
            global.check_types(self, &mut dependencies)?;
            if !dependencies.is_empty() {
                Err(ZeptwoError::parser_error_at_pos(global.left_pos(self), "Initial value of global variable can not call user defined functions."))?
            }
        }
        let mut functions = self.function_id_vec.clone();
        for function in &mut functions {
            function.check_types(self, &mut dependencies)?;
        }
        self.function_id_vec = functions;
        Ok(())
    }
    pub fn determine_size_and_indexes(&mut self) -> Result<(), ZeptwoError> {
        let mut global_index = 0;
        for global in &mut self.global_variable_definitions.clone() {
            global.determine_size_and_indexes(&mut global_index, self)?;
        }

        let mut functions = self.function_id_vec.clone();
        for function in &mut functions {
            let mut stack_index = 0;
            function.determine_size_and_indexes(&mut stack_index, self)?;
        }
        self.function_id_vec = functions;
        Ok(())
    }
    pub fn new_variable(
        &mut self,
        lexeme: &'static str,
        data_type: Option<ValType>,
        pos: Pos,
        is_global: bool
    ) -> VariableID {
        let id = VariableID::next_id(&self.variable_info_vec);

        let scope_identifier_map = self.scope_identifier_map.last_mut().unwrap();
        scope_identifier_map.insert(lexeme, IdentifierID::Var(id));

        self.variable_info_vec.push(VariableInfo {
            name: lexeme,
            data_type,
            stack_index: None,
            declared_pos: pos,
            is_global,
        });

        id
    }

    pub fn start_scope(&mut self) {
        self.scope_identifier_map.push(HashMap::new())
    }

    pub fn end_scope(&mut self) {
        self.scope_identifier_map.pop();
    }

    pub fn new_function(&self) {
        if self.scope_identifier_map.len() != 1 {
            println!("len: {}", self.scope_identifier_map.len());
            panic!("String map contains more than global scope.")
        }
    }

    pub fn var_pos(&self, id: VariableID) -> Pos {
        self.variable_info_vec[id].declared_pos
    }

    pub fn new_ambiguous(&mut self, function_id: FunctionID) -> AmbiguousFunctionID {
        let name = self.function_id_vec[function_id].name;
        match self.ambiguous_function_map.get(name) {
            Some(id) => {
                self.ambiguous_function_vec[*id].push(function_id);
                *id
            }
            None => {
                let id = AmbiguousFunctionID::next_id(&self.ambiguous_function_vec);
                self.ambiguous_function_vec.push(vec![function_id]);
                self.ambiguous_function_map.insert(name, id);
                id
            }
        }
    }
    pub fn get_ambiguous_name(&mut self, id: AmbiguousFunctionID) -> &'static str {
        self.function_id_vec[self.ambiguous_function_vec[id][0]].name
    }
    pub fn as_expr(&self) -> Expr {
        todo!()
    }

    pub fn define_function(
        &mut self,
        name: &'static str,
        params: Vec<VariableID>,
        body: Expr,
        return_type: ValType,
        declared_pos: Pos,
    ) {
        let id = FunctionID::next_id(&self.function_id_vec);
        let mut param_types = vec![];
        for param in &params {
            param_types.push(param.get_variable_type(self).unwrap());
        }
        self.function_id_vec.push(FunctionInfo {
            id,
            name,
            params,
            param_types,
            body,
            return_type,
            declared_pos,
            stack_size: None,
            start_adress: None,
            dependencies: vec![],
            is_compiled: false,
        });
        let ambig_id = self.new_ambiguous(id);

        let string_map = self.scope_identifier_map.last_mut().unwrap();
        string_map.insert(name, IdentifierID::Func(ambig_id));
    }
    pub fn determine_function(
        &self,
        ambig_callee: AmbiguousFunctionID,
        types: &[ValType],
    ) -> Option<FunctionID> {
        let ambig_functions = &self.ambiguous_function_vec[ambig_callee];

        for function in ambig_functions {
            let function_info = &self.function_id_vec[*function];
            if function_info.param_types == types {
                return Some(*function);
            }
        }
        None
    }

    pub fn acquire_identifier_id(
        &mut self,
        lexeme: &str,
    ) -> Option<IdentifierID> {
        let current_scope = self.scope_identifier_map.last_mut().unwrap();
        match current_scope.get(lexeme) {
            Some(old_id) => Some(*old_id),
            None => {
                let len = self.scope_identifier_map.len() - 1;
                Self::acquire_identifier_id_from_vec(
                    lexeme,
                    &mut self.scope_identifier_map[0..len],
                )
            }
        }
    }

    fn acquire_identifier_id_from_vec(
        lexeme: &str,
        string_map: &mut [HashMap<&'static str, IdentifierID>],
    ) -> Option<IdentifierID> {
        let current_scope = string_map.last_mut();
        match current_scope {
            None => None,
            Some(current_scope) => {
                match current_scope.get(lexeme) {
                    Some(old_id) => Some(*old_id),
                    None => {
                        let len = string_map.len() - 1;
                        Self::acquire_identifier_id_from_vec(lexeme, &mut string_map[0..len])
                    }
                }
            }
        }
        
    }
}

#[derive(Clone)]
pub struct VariableInfo {
    pub name: &'static str,
    pub data_type: Option<ValType>,
    pub stack_index: Option<usize>,
    pub declared_pos: Pos,
    pub is_global: bool,
}

newtype_index!(VariableID, VariableInfo);

impl VariableID {
    pub fn get_variable_info(self, lookup: &IdentifierLookup) -> VariableInfo {
        lookup.variable_info_vec[self].clone()
    }

    pub fn get_variable_type(self, lookup: &IdentifierLookup) -> Option<ValType> {
        lookup.variable_info_vec[self].data_type.as_ref().cloned()
    }

    pub fn get_variable_name(self, lookup: &IdentifierLookup) -> &'static str {
        lookup.variable_info_vec[self].name
    }

    pub fn set_stack_index(self, stack_index: usize, lookup: &mut IdentifierLookup) {
        lookup.variable_info_vec[self].stack_index = Some(stack_index);
    }

    pub fn set_variable_type(self, val_type: Option<&ValType>, lookup: &mut IdentifierLookup) {
        lookup.variable_info_vec[self].data_type = val_type.cloned();
    }
}

impl Position for VariableID {
    fn left_pos(&self, lookup: &IdentifierLookup) -> Pos {
        lookup.variable_info_vec[*self].declared_pos
    }
    fn right_pos(&self, lookup: &IdentifierLookup) -> Pos {
        self.left_pos(lookup)
    }
}
impl SizeAndIndex for VariableID {
    fn determine_size_and_indexes(
        &mut self,
        stack_index: &mut usize,
        lookup: &mut IdentifierLookup,
    ) -> Result<usize, ZeptwoError> {
        self.set_stack_index(*stack_index, lookup);
        let size = self
            .get_variable_type(lookup)
            .unwrap()
            .get_result_size(lookup) as usize;
        *stack_index += size;
        Ok(size)
    }
}

newtype_index!(AmbiguousFunctionID, Vec<FunctionID>);

newtype_index!(FunctionID, FunctionInfo);

impl FunctionID {
    pub fn get_name(self, lookup: &IdentifierLookup) -> &'static str {
        lookup.function_id_vec[self].name
    }
    pub fn get_return_type(self, lookup: &IdentifierLookup) -> ValType {
        lookup.function_id_vec[self].return_type.clone()
    }
    pub fn get_address(self, lookup: &IdentifierLookup) -> Option<usize> {
        lookup.function_id_vec[self].start_adress
    }
}

#[derive(Clone)]
pub struct FunctionInfo {
    pub id: FunctionID,
    pub name: &'static str, // todo: hide name and create display method where function types can be seen
    params: Vec<VariableID>,
    param_types: Vec<ValType>,
    pub body: Expr,
    return_type: ValType,
    pub stack_size: Option<usize>,
    pub declared_pos: Pos,
    pub start_adress: Option<usize>,
    pub dependencies: Vec<DependencyID>,
    pub is_compiled: bool,
}

impl FunctionInfo {
    pub fn is_main(&mut self) -> bool {
        self.name == "main" && self.params.is_empty() && self.return_type == ValType::Int
    }
}

impl CheckTypes for FunctionInfo {
    fn check_types(&mut self, lookup: &mut IdentifierLookup, dependencies: &mut Vec<DependencyID>) -> Result<(), ZeptwoError> {
        if self.return_type != self.body.determine_type_and_opcode(lookup, &mut self.dependencies)? {
            Err(ZeptwoError::parser_error(format!(
                "Expected function '{}' to return type '{}'",
                self.name, self.return_type
            )))?;
        }
        dependencies.extend_from_slice(self.dependencies.as_slice());
        Ok(())
    }
}

impl SizeAndIndex for FunctionInfo {
    fn determine_size_and_indexes(
        &mut self,
        stack_index: &mut usize,
        lookup: &mut IdentifierLookup,
    ) -> Result<usize, ZeptwoError> {
        let mut total_size = 0;

        for param in &mut self.params {
            // TODO: i am not sure if this should be included in total size.
            total_size += param.determine_size_and_indexes(stack_index, lookup)?;
        }
        total_size += self.body.determine_size_and_indexes(stack_index, lookup)?;

        self.stack_size = Some(total_size);
        println!("name: {}, size: {}", self.name, self.stack_size.unwrap());
        Ok(total_size)
    }
}

#[derive(Clone, Copy)]
pub enum IdentifierID {
    Var(VariableID),
    Func(AmbiguousFunctionID),
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub enum DependencyID {
    // Var(VariableID), // TODO: MIght remove
    Func(FunctionID),
}