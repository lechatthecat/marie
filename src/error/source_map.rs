use serde::Serialize;
use std::collections::HashMap;
use serde_json::Result as JsonResult; 

#[derive(Serialize, Default)]
pub struct SourceMap {
    pub map: HashMap<String, Vec<String>>,
}

impl SourceMap {
    // Converts the SourceMap to a JSON string
    fn to_json_stringify(&self) -> JsonResult<String> {
        serde_json::to_string(&self)
    }
}
