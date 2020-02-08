pub mod lexer;

pub use lexer::{TokenType, Token};

use std::string::String;

pub trait HoaConsumer {
    fn set_name(&mut self, name: &String);
    fn set_aps(&mut self, aps: &Vec<String>);
    // parameter is int_list which is typedefed to std::vector<unsigned int>
    fn add_start_states(&mut self, st_conj: &Vec<usize>);
    // has second argument std::vector<IntOrString>
    fn provide_acceptance_name(&mut self, name: &String);
    fn notify_body_start(&mut self);
    // additional argument label_expr::ptr is ignored
    fn add_state(&mut self, id: usize, info: &String, acc_sig: &Vec<usize>);
    fn add_edge_implicit(&mut self, sig: usize, conj_sucs: &Vec<usize>);
    // additional argument label_expr::ptr ignored
    fn add_edge_with_label(&mut self, sid: usize, int_list: &Vec<usize>, acc_sig: &Vec<usize>);
    fn notify_end_of_state(&mut self, sid: usize);
    fn notify_end(&mut self);
    fn notify_warning(&self, warning: &String);
}

pub struct HoaParser {

}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
