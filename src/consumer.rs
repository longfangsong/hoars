use crate::parser::{AcceptanceCondition, BooleanExpressionAlias};

pub trait HoaConsumer {
    fn notify_header_start(&mut self, version: &String);
    fn set_name(&mut self, name: &String);
    fn set_aps(&mut self, aps: Vec<String>);
    fn set_number_of_states(&mut self, number: usize);
    fn add_properties(&mut self, property_list: Vec<String>);
    // parameter is int_list which is typedefed to std::vector<unsigned int>
    fn add_start_states(&mut self, st_conj: Vec<usize>);
    // has second argument std::vector<IntOrString>
    fn provide_acceptance_name(&mut self, name: &String);
    fn notify_body_start(&mut self);
    // additional argument label_expr::ptr is ignored
    fn add_state(
        &mut self,
        id: usize,
        info: Option<&String>,
        label_expr: Option<&BooleanExpressionAlias>,
        acc_sig: Option<&Vec<usize>>,
    );
    fn add_edge_implicit(
        &mut self,
        sig: usize,
        conj_sucs: &Vec<usize>,
        acc_sig: Option<&Vec<usize>>,
    );
    // additional argument label_expr::ptr ignored
    fn add_edge_with_label(
        &mut self,
        sid: usize,
        label_expr: &BooleanExpressionAlias,
        int_list: &Vec<usize>,
        acc_sig: Option<&Vec<usize>>,
    );
    fn notify_end_of_state(&mut self, sid: usize);
    fn notify_end(&mut self);
    fn notify_warning(&self, warning: &String);
    fn add_alias(&mut self, alias_name: &String, alias_expr: &BooleanExpressionAlias);
    fn set_acceptance_condition(
        &mut self,
        number_of_sets: usize,
        acceptance_expr: &AcceptanceCondition,
    );
    fn set_tool(&mut self, tool_info: Vec<String>);
}

pub struct PrintConsumer {}
pub struct NopConsumer {}

impl HoaConsumer for NopConsumer {
    fn notify_header_start(&mut self, version: &String) {}

    fn set_name(&mut self, name: &String) {}

    fn set_aps(&mut self, aps: Vec<String>) {}

    fn set_number_of_states(&mut self, number: usize) {}

    fn add_properties(&mut self, property_list: Vec<String>) {}

    fn add_start_states(&mut self, st_conj: Vec<usize>) {}

    fn provide_acceptance_name(&mut self, name: &String) {}

    fn notify_body_start(&mut self) {}

    fn add_state(
        &mut self,
        id: usize,
        info: Option<&String>,
        label_expr: Option<&BooleanExpressionAlias>,
        acc_sig: Option<&Vec<usize>>,
    ) {
    }

    fn add_edge_implicit(
        &mut self,
        sig: usize,
        conj_sucs: &Vec<usize>,
        acc_sig: Option<&Vec<usize>>,
    ) {
    }

    fn add_edge_with_label(
        &mut self,
        sid: usize,
        label_expr: &BooleanExpressionAlias,
        int_list: &Vec<usize>,
        acc_sig: Option<&Vec<usize>>,
    ) {
    }

    fn notify_end_of_state(&mut self, sid: usize) {}

    fn notify_end(&mut self) {}

    fn notify_warning(&self, warning: &String) {}

    fn add_alias(&mut self, alias_name: &String, alias_expr: &BooleanExpressionAlias) {}

    fn set_acceptance_condition(
        &mut self,
        number_of_sets: usize,
        acceptance_expr: &AcceptanceCondition,
    ) {
    }

    fn set_tool(&mut self, tool_info: Vec<String>) {}
}

impl HoaConsumer for PrintConsumer {
    fn notify_header_start(&mut self, version: &String) {
        println!("notified of header start, version: {}", version);
    }

    fn set_name(&mut self, name: &String) {
        println!("name was set to {}", name);
    }

    fn set_aps(&mut self, aps: Vec<String>) {
        print!("atomic propositions set: [");
        for ap in aps {
            print!(" {} ", ap);
        }
        println!("]");
    }

    fn set_number_of_states(&mut self, number: usize) {
        println!("number of states set to {}", number);
    }

    fn add_properties(&mut self, property_list: Vec<String>) {
        print!("properties");
        for property in property_list {
            print!(" {} ", property);
        }
        println!("set");
    }

    fn add_start_states(&mut self, st_conj: Vec<usize>) {
        print!("start states {{");
        for state in st_conj {
            print!(" {} ", state);
        }
        println!("}} set");
    }

    fn provide_acceptance_name(&mut self, name: &String) {
        println!("set acceptance name {}", name);
    }

    fn notify_body_start(&mut self) {
        println!("---start of body---");
    }

    fn add_state(
        &mut self,
        id: usize,
        info: Option<&String>,
        label_expr: Option<&BooleanExpressionAlias>,
        acc_sig: Option<&Vec<usize>>,
    ) {
        println!("adding state with id {}", id);
        if let Some(i) = info {
            println!("\twith info {}", i);
        }
        if let Some(le) = label_expr {
            println!("\twith label {}", le);
        }
        if let Some(acs) = acc_sig {
            print!("\twith acc_sig [");
            for a in acs {
                print!(" {} ", a);
            }
            println!("]");
        }
    }

    fn add_edge_implicit(
        &mut self,
        sig: usize,
        conj_sucs: &Vec<usize>,
        acc_sig: Option<&Vec<usize>>,
    ) {
        println!("\tadding implicit edge for state {}", sig);
        print!("\t\twith conj_sucs [");
        for cs in conj_sucs {
            print!(" {} ", cs);
        }
        println!("]");
        if let Some(acs) = acc_sig {
            print!("\t\twith acc_sig {{");
            for a in acs {
                print!(" {} ", a);
            }
            println!("}}");
        }
    }

    fn add_edge_with_label(
        &mut self,
        sid: usize,
        label_expr: &BooleanExpressionAlias,
        int_list: &Vec<usize>,
        acc_sig: Option<&Vec<usize>>,
    ) {
        println!("\tadding labelled edge for state {}", sid);
        println!("\t\twith label {}", label_expr);
        print!("\t\twith int_list [");
        for i in int_list {
            print!(" {} ", i);
        }
        println!("]");
        if let Some(acs) = acc_sig {
            print!("\t\twith acc_sig {{");
            for a in acs {
                print!(" {} ", a);
            }
            println!("}}");
        }
    }

    fn notify_end_of_state(&mut self, sid: usize) {
        println!("end of state {}", sid);
    }

    fn notify_end(&mut self) {
        println!("---end of body---");
    }

    fn notify_warning(&self, warning: &String) {
        println!("---WARNING---");
    }

    fn add_alias(&mut self, alias_name: &String, alias_expr: &BooleanExpressionAlias) {
        println!(
            "adding alias @{} with expression {}",
            alias_name, alias_expr
        );
    }

    fn set_acceptance_condition(
        &mut self,
        number_of_sets: usize,
        acceptance_expr: &AcceptanceCondition,
    ) {
        println!(
            "adding acceptance condition {} with {} sets",
            acceptance_expr, number_of_sets
        );
    }

    fn set_tool(&mut self, tool_info: Vec<String>) {
        println!("setting tool");
        for ti in tool_info {
            println!("\t{}", ti);
        }
    }
}
