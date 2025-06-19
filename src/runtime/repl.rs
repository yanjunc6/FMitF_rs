//! REPL Interface - Simple interactive runtime for TransAct testing

use super::{RuntimeState, ReplCommand, RuntimeError};
use clap_repl::{ClapEditor, ReadCommandOutput};
use std::sync::{Arc, Mutex};

/// Start the interactive runtime REPL with auto-completion
pub fn start_runtime_repl() -> Result<(), String> {
    println!("🚀 TransAct Testing REPL");
    println!("Type 'help' for commands, TAB for completion, Ctrl+C or 'exit' to quit");
    println!("Available commands: load, call, table, functions, tables, exit, help");
    println!();
    
    let runtime_state = Arc::new(Mutex::new(RuntimeState::new()));
    
    // Create the clap-repl editor with basic auto-completion
    let mut editor = ClapEditor::<ReplCommand>::builder()
        .with_editor_hook(move |editor| {
            // clap-repl provides basic command completion automatically
            // This hook allows us to customize the editor if needed
            editor
        })
        .build();
    
    // Manual REPL loop with proper exit handling
    loop {
        match editor.read_command() {
            ReadCommandOutput::Command(command) => {
                match &command {
                    ReplCommand::Exit => {
                        println!("👋 Goodbye!");
                        break;
                    }
                    _ => {
                        let mut state = runtime_state.lock().unwrap();
                        if let Err(e) = handle_command(command, &mut state) {
                            eprintln!("❌ Error: {}", e);
                        }
                    }
                }
            }
            ReadCommandOutput::EmptyLine => {
                // Just continue on empty lines
            }
            ReadCommandOutput::ClapError(err) => {
                println!("{}", err);
            }
            ReadCommandOutput::ShlexError => {
                eprintln!("❌ Invalid input: unmatched quotes");
            }
            ReadCommandOutput::ReedlineError(err) => {
                eprintln!("❌ Input error: {}", err);
                break;
            }
            ReadCommandOutput::CtrlC => {
                println!("\n👋 Goodbye!");
                break;
            }
            ReadCommandOutput::CtrlD => {
                println!("\n👋 Goodbye!");
                break;
            }
        }
    }
    
    Ok(())
}

/// Handle a parsed REPL command
fn handle_command(command: ReplCommand, state: &mut RuntimeState) -> Result<(), RuntimeError> {
    match command {
        ReplCommand::Load { file } => {
            println!("Loading: {}", file);
            state.load_file(&file)?;
        }
        
        ReplCommand::Call { function, args } => {
            println!("Executing: {} {:?}", function, args);
            state.call_function(&function, args)?;
        }
        
        ReplCommand::Table { name } => {
            println!("Table: {}", name);
            state.print_table(&name)?;
        }
        
        ReplCommand::Functions => {
            let functions = state.list_functions();
            if functions.is_empty() {
                println!("No functions loaded. Use 'load <file>' first.");
            } else {
                println!("Available functions:");
                for func in functions {
                    println!("  - {}", func);
                }
            }
        }
        
        ReplCommand::Tables => {
            let tables = state.list_tables();
            if tables.is_empty() {
                println!("No tables loaded. Use 'load <file>' first.");
            } else {
                println!("Available tables:");
                for table in tables {
                    println!("  - {}", table);
                }
            }
        }
        
        ReplCommand::Exit => {
            // This should be handled in the main loop, but just in case
            std::process::exit(0);
        }
    }
    
    Ok(())
}