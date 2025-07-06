use crate::bytecode::values::value::{self, Value};
use std::collections::HashMap;
use std::mem::size_of_val;

enum GCData {
    String(String),
    Closure(value::Closure),
    Class(value::Class),
    Instance(value::Instance),
    BoundMethod(value::BoundMethod),
    List(Vec<Value>),
}

impl GCData {
    fn as_str(&self) -> Option<&String> {
        match self {
            GCData::String(s) => Some(s),
            _ => None,
        }
    }
    fn as_list(&self) -> Option<&Vec<Value>> {
        match self {
            GCData::List(elements) => Some(elements),
            _ => None,
        }
    }
    fn as_list_mut(&mut self) -> Option<&mut Vec<Value>> {
        match self {
            GCData::List(elements) => Some(elements),
            _ => None,
        }
    }
    fn as_closure(&self) -> Option<&value::Closure> {
        match self {
            GCData::Closure(c) => Some(c),
            _ => None,
        }
    }
    fn as_mut_closure(&mut self) -> Option<&mut value::Closure> {
        match self {
            GCData::Closure(c) => Some(c),
            _ => None,
        }
    }
    fn as_bound_method(&self) -> Option<&value::BoundMethod> {
        match self {
            GCData::BoundMethod(m) => Some(m),
            _ => None,
        }
    }
    fn as_class(&self) -> Option<&value::Class> {
        match self {
            GCData::Class(c) => Some(c),
            _ => None,
        }
    }
    fn as_class_mut(&mut self) -> Option<&mut value::Class> {
        match self {
            GCData::Class(c) => Some(c),
            _ => None,
        }
    }
    fn as_instance(&self) -> Option<&value::Instance> {
        match self {
            GCData::Instance(inst) => Some(inst),
            _ => None,
        }
    }
    fn as_instance_mut(&mut self) -> Option<&mut value::Instance> {
        match self {
            GCData::Instance(inst) => Some(inst),
            _ => None,
        }
    }
}

pub struct GCVal {
    is_marked: bool,
    data: GCData,
    class_id: Option<usize>,
}

impl GCVal {
    fn from(data: GCData) -> GCVal {
        match data {
            GCData::Instance(val) => GCVal {
                is_marked: false,
                data: GCData::Instance(val.clone()),
                class_id: Some(val.class_id),
            },
            GCData::BoundMethod(val) => GCVal {
                is_marked: false,
                data: GCData::BoundMethod(val.clone()),
                class_id: Some(val.closure_id),
            },
            _ => GCVal {
                is_marked: false,
                data,
                class_id: None,
            },
        }
    }
}

pub type HeapId = usize;

pub struct Heap {
    bytes_allocated: usize,
    next_gc: usize,
    id_counter: usize,
    pub values: HashMap<HeapId, GCVal>,
}

impl Default for Heap {
    fn default() -> Heap {
        let next_gc = std::env::var("MARIE_GC_TRIGGER_SIZE")
            .ok()
            .and_then(|env_str| env_str.parse::<usize>().ok())
            .unwrap_or(1024 * 1024);
        Heap {
            bytes_allocated: 0,
            next_gc,
            id_counter: 0,
            values: Default::default(),
        }
    }
}

impl Heap {
    pub fn summarize_stats(&self) -> String {
        format!(
            "Heap stats: bytes_allocated {}\n\
                             next_gc {}\n\
                             num_values: {}",
            self.bytes_allocated,
            self.next_gc,
            self.values.len()
        )
    }

    pub fn manage_str(&mut self, s: String) -> HeapId {
        self.bytes_allocated += s.len();
        let id = self.generate_id();
        self.values.insert(id, GCVal::from(GCData::String(s)));
        id
    }

    pub fn manage_list(&mut self, elements: Vec<Value>) -> HeapId {
        self.bytes_allocated += elements.len();
        let id = self.generate_id();
        self.values.insert(id, GCVal::from(GCData::List(elements)));
        id
    }

    pub fn manage_closure(&mut self, c: value::Closure) -> HeapId {
        self.bytes_allocated += c.function.chunk.code.len();
        self.bytes_allocated += c.function.chunk.constants.len();
        let id = self.generate_id();
        self.values.insert(id, GCVal::from(GCData::Closure(c)));
        id
    }

    pub fn manage_class(&mut self, c: value::Class) -> HeapId {
        let id = self.generate_id();
        self.bytes_allocated += c.name.len();
        let sum: usize = c
            .properties
            .keys()
            .map(|marieval| size_of_val(marieval))
            .sum();
        self.bytes_allocated += sum;
        let sum: usize = c
            .properties
            .values()
            .map(|marieval| size_of_val(marieval))
            .sum();
        self.bytes_allocated += sum;
        self.values.insert(id, GCVal::from(GCData::Class(c)));
        id
    }

    pub fn manage_instance(&mut self, inst: value::Instance) -> HeapId {
        let id = self.generate_id();
        self.bytes_allocated += inst
            .fields
            .keys()
            .map(|attr| attr.name.len())
            .sum::<usize>();
        self.values.insert(id, GCVal::from(GCData::Instance(inst)));
        id
    }

    pub fn manage_bound_method(&mut self, method: value::BoundMethod) -> HeapId {
        let id = self.generate_id();
        self.values
            .insert(id, GCVal::from(GCData::BoundMethod(method)));
        id
    }

    fn generate_id(&mut self) -> HeapId {
        self.id_counter += 1;
        loop {
            if !self.values.contains_key(&self.id_counter) {
                return self.id_counter;
            }
            self.id_counter += 1;
        }
    }

    pub fn _get_class_id(&self, id: HeapId) -> usize {
        self.values.get(&id).unwrap().class_id.unwrap()
    }

    pub fn _set_class_id(&mut self, id: HeapId, class_id: usize) {
        self.values.get_mut(&id).unwrap().class_id = Some(class_id);
    }

    pub fn get_gcval(&self, id: HeapId) -> &GCVal {
        self.values.get(&id).unwrap()
    }

    pub fn get_str(&self, id: HeapId) -> &String {
        let val = self.values.get(&id).unwrap();
        val.data.as_str().unwrap()
    }

    pub fn get_closure(&self, id: HeapId) -> &value::Closure {
        self.values.get(&id).unwrap().data.as_closure().unwrap()
    }

    pub fn get_mut_closure(&mut self, id: HeapId) -> &mut value::Closure {
        self.values.get_mut(&id).unwrap().data.as_mut_closure().unwrap()
    }

    pub fn get_bound_method(&self, id: HeapId) -> &value::BoundMethod {
        self.values
            .get(&id)
            .unwrap()
            .data
            .as_bound_method()
            .unwrap()
    }

    pub fn get_list_elements(&self, id: HeapId) -> &Vec<Value> {
        self.values.get(&id).unwrap().data.as_list().unwrap()
    }

    pub fn get_list_elements_mut(&mut self, id: HeapId) -> &mut Vec<Value> {
        self.values
            .get_mut(&id)
            .unwrap()
            .data
            .as_list_mut()
            .unwrap()
    }

    pub fn get_class(&self, id: HeapId) -> &value::Class {
        self.values.get(&id).unwrap().data.as_class().unwrap()
    }

    pub fn get_class_mut(&mut self, id: HeapId) -> &mut value::Class {
        self.values
            .get_mut(&id)
            .unwrap()
            .data
            .as_class_mut()
            .unwrap()
    }

    pub fn get_class_mut_and_set_class_id(
        &mut self,
        class_id: HeapId,
        attr_id: usize,
    ) -> &mut value::Class {
        self.values.get_mut(&attr_id).unwrap().class_id = Some(class_id);
        self.values
            .get_mut(&class_id)
            .unwrap()
            .data
            .as_class_mut()
            .unwrap()
    }

    pub fn get_instance(&self, id: HeapId) -> &value::Instance {
        self.values.get(&id).unwrap().data.as_instance().unwrap()
    }

    pub fn get_instance_mut(&mut self, id: HeapId) -> &mut value::Instance {
        self.values
            .get_mut(&id)
            .unwrap()
            .data
            .as_instance_mut()
            .unwrap()
    }

    pub fn unmark(&mut self) {
        for val in self.values.values_mut() {
            val.is_marked = false;
        }
    }

    pub fn mark(&mut self, id: HeapId) {
        self.values.get_mut(&id).unwrap().is_marked = true;
    }

    pub fn is_marked(&self, id: HeapId) -> bool {
        self.values.get(&id).unwrap().is_marked
    }

    pub fn children(&self, id: HeapId) -> Vec<HeapId> {
        match &self.values.get(&id).unwrap().data {
            GCData::String(_) => Vec::new(),
            GCData::Closure(_closure) => Vec::new(), // TODO: closure upval
            GCData::Class(class) => self.class_children(class),
            GCData::Instance(instance) => self.instance_children(instance),
            GCData::BoundMethod(method) => self.bound_method_children(method),
            GCData::List(elements) => self.list_children(elements),
        }
    }

    pub fn bound_method_children(&self, method: &value::BoundMethod) -> Vec<HeapId> {
        vec![method.instance_id, method.closure_id]
    }

    pub fn class_children(&self, class: &value::Class) -> Vec<HeapId> {
        class
            .properties
            .values()
            .filter(|marieval| match marieval {
                value::Value::String(_heapid) => true,
                value::Value::Function(_heapid) => true,
                value::Value::Instance(_heapid) => true,
                value::Value::BoundMethod(_heapid) => true,
                value::Value::Class(_heapid) => true,
                value::Value::List(_heapid) => true,
                _ => false,
            })
            .map(|marieval| match &marieval {
                value::Value::String(heapid) => &heapid,
                value::Value::Function(heapid) => &heapid,
                value::Value::Instance(heapid) => &heapid,
                value::Value::BoundMethod(heapid) => &heapid,
                value::Value::Class(heapid) => &heapid,
                value::Value::List(heapid) => &heapid,
                _ => &0,
            })
            .copied()
            .collect()
    }

    pub fn instance_children(&self, instance: &value::Instance) -> Vec<HeapId> {
        let mut res = vec![instance.class_id];

        for field in instance.fields.values() {
            if let Some(id) = Heap::extract_id(&field) {
                res.push(id)
            }
        }

        res
    }

    pub fn list_children(&self, elements: &[Value]) -> Vec<HeapId> {
        let mut res = Vec::new();

        for element in elements {
            if let Some(id) = Heap::extract_id(&element) {
                res.push(id)
            }
        }

        res
    }

    pub fn extract_id(val: &value::Value) -> Option<HeapId> {
        match val {
            value::Value::Number(_) => None,
            value::Value::Bool(_) => None,
            value::Value::String(id) => Some(*id),
            value::Value::Function(id) => Some(*id),
            value::Value::Instance(id) => Some(*id),
            value::Value::BoundMethod(id) => Some(*id),
            value::Value::Class(id) => Some(*id),
            value::Value::NativeFunction(_) => None,
            value::Value::Null => None,
            value::Value::List(id) => Some(*id),
        }
    }

    pub fn mutable_extract_id(val: &Value) -> Option<HeapId> {
        match val {
            value::Value::Number(_) => None,
            value::Value::Bool(_) => None,
            value::Value::String(id) => Some(*id),
            value::Value::Function(id) => Some(*id),
            value::Value::Instance(id) => Some(*id),
            value::Value::BoundMethod(id) => Some(*id),
            value::Value::Class(id) => Some(*id),
            value::Value::NativeFunction(_) => None,
            value::Value::Null => None,
            value::Value::List(id) => Some(*id),
        }
    }

    pub fn sweep(&mut self) {
        self.values.retain(|_, val| val.is_marked)
    }

    pub fn should_collect(&self) -> bool {
        self.bytes_allocated >= self.next_gc
    }
}
