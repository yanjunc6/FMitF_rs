//! REPL Interface - Simple interactive runtime for TransAct testing

use super::{RuntimeState, ReplCommand, RuntimeError};
use crate::cfg::CfgProgram;
use clap_repl::{ClapEditor, ReadCommandOutput};
use clap_repl::reedline::{Completer, Suggestion, Span};
use std::sync::{Arc, Mutex};


struct TransActCompleter {
    runtime_state: Arc<Mutex<RuntimeState>>,
}

impl TransActCompleter {
    fn new(runtime_state: Arc<Mutex<RuntimeState>>) -> Self {
        Self { runtime_state }
    }
}

impl Completer for TransActCompleter {
    fn complete(&mut self, line: &str, pos: usize) -> Vec<Suggestion> {
        let mut suggestions = Vec::new();
        let words: Vec<&str> = line[..pos].split_whitespace().collect();
        
        // Check if we're completing a command name or command arguments
        if words.is_empty() || (words.len() == 1 && !line[..pos].ends_with(' ')) {
            // Complete command names
            let commands = ["call", "table", "functions", "tables", "clear", "exit"];
            let prefix = words.first().copied().unwrap_or("");
            
            for &cmd in &commands {
                if cmd.starts_with(prefix) {
                    suggestions.push(Suggestion {
                        value: cmd.to_string(),
                        description: match cmd {
                            "call" => Some("Call a function".to_string()),
                            "table" => Some("Show table contents".to_string()),
                            "functions" => Some("List all functions".to_string()),
                            "tables" => Some("List all tables".to_string()),
                            "clear" => Some("Clear all table data".to_string()),
                            "exit" => Some("Exit REPL".to_string()),
                            _ => None,
                        },
                        style: None,
                        extra: None,
                        span: Span::new(pos.saturating_sub(prefix.len()), pos),
                        append_whitespace: true,
                    });
                }
            }
        } else if words.len() >= 1 {
            // Complete command arguments
            match words[0] {
                "call" => {
                    // Complete function names
                    if let Ok(state) = self.runtime_state.lock() {
                        let functions = state.list_functions();
                        let prefix = if words.len() >= 2 { words[1] } else { "" };
                        
                        for func in &functions {
                            if func.starts_with(prefix) {
                                let start_pos = if words.len() >= 2 {
                                    pos.saturating_sub(prefix.len())
                                } else {
                                    pos
                                };
                                suggestions.push(Suggestion {
                                    value: func.clone(),
                                    description: Some("function".to_string()),
                                    style: None,
                                    extra: None,
                                    span: Span::new(start_pos, pos),
                                    append_whitespace: true,
                                });
                            }
                        }
                    }
                }
                "table" => {
                    // Complete table names
                    if let Ok(state) = self.runtime_state.lock() {
                        let tables = state.list_tables();
                        let prefix = if words.len() >= 2 { words[1] } else { "" };
                        
                        for table in &tables {
                            if table.starts_with(prefix) {
                                let start_pos = if words.len() >= 2 {
                                    pos.saturating_sub(prefix.len())
                                } else {
                                    pos
                                };
                                suggestions.push(Suggestion {
                                    value: table.clone(),
                                    description: Some("table".to_string()),
                                    style: None,
                                    extra: None,
                                    span: Span::new(start_pos, pos),
                                    append_whitespace: true,
                                });
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        
        suggestions
    }
}

/// Start the interactive runtime REPL with auto-completion
pub fn start_runtime_repl() -> Result<(), String> {
    use colored::*;
    
    println!("{}", "TransAct Testing REPL".bright_blue().bold());
    println!("Type {} for commands, {} for completion, {} or {} to quit",
        "'help'".bright_cyan(),
        "TAB".bright_yellow(), 
        "Ctrl+C".bright_magenta(),
        "'exit'".bright_cyan()
    );
    println!("Available commands: {}", 
        "call, table, functions, tables, clear, exit, help".bright_white());
    println!();
    
    let runtime_state = Arc::new(Mutex::new(RuntimeState::new()));
    let completer = TransActCompleter::new(runtime_state.clone());
    
    // Create the clap-repl editor with custom auto-completion
    let mut editor = ClapEditor::<ReplCommand>::builder()
        .with_editor_hook(move |reedline| {
            // Add the custom completer
            reedline.with_completer(Box::new(completer))
        })
        .build();
    
    // Manual REPL loop with proper exit handling
    loop {
        match editor.read_command() {
            ReadCommandOutput::Command(command) => {
                match &command {
                    ReplCommand::Exit => {
                        println!("Goodbye!");
                        break;
                    }
                    _ => {
                        let mut state = runtime_state.lock().unwrap();
                        if let Err(e) = handle_command(command, &mut state) {
                            eprintln!("ERROR: {}", e);
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
                eprintln!("ERROR: Invalid input: unmatched quotes");
            }
            ReadCommandOutput::ReedlineError(err) => {
                eprintln!("ERROR: Input error: {}", err);
                break;
            }
            ReadCommandOutput::CtrlC => {
                println!("\nGoodbye!");
                break;
            }
            ReadCommandOutput::CtrlD => {
                println!("\nGoodbye!");
                break;
            }
        }
    }
    
    Ok(())
}

/// Start the interactive runtime REPL with a pre-loaded optimized CFG
pub fn start_runtime_repl_with_cfg(cfg_program: CfgProgram) -> Result<(), String> {
    use colored::*;
    
    println!("{}", "TransAct Interactive Runtime".bright_blue().bold());
    println!("Loaded optimized CFG with {} functions and {} tables", 
             cfg_program.functions.len().to_string().bright_cyan(), 
             cfg_program.tables.len().to_string().bright_cyan());
    println!("Type {} for commands, {} for completion, {} or {} to quit",
        "'help'".bright_cyan(),
        "TAB".bright_yellow(),
        "Ctrl+C".bright_magenta(),
        "'exit'".bright_cyan()
    );
    println!("Available commands: {}", 
        "call, table, functions, tables, clear, exit, help".bright_white());
    println!("Note: Database starts empty - use functions to populate data, 'clear' to reset");
    println!();
    
    let mut runtime_state = RuntimeState::new();
    
    // Load the CFG into the runtime state
    runtime_state.load_cfg(cfg_program).map_err(|e| format!("Failed to load CFG: {}", e))?;
    
    let runtime_state = Arc::new(Mutex::new(runtime_state));
    let completer = TransActCompleter::new(runtime_state.clone());
    
    // Create the clap-repl editor with custom auto-completion
    let mut editor = ClapEditor::<ReplCommand>::builder()
        .with_editor_hook(move |reedline| {
            // Add the custom completer
            reedline.with_completer(Box::new(completer))
        })
        .build();
    
    // Manual REPL loop with proper exit handling
    loop {
        match editor.read_command() {
            ReadCommandOutput::Command(command) => {
                match &command {
                    ReplCommand::Exit => {
                        println!("Goodbye!");
                        break;
                    }
                    _ => {
                        let mut state = runtime_state.lock().unwrap();
                        if let Err(e) = handle_command(command, &mut state) {
                            eprintln!("ERROR: {}", e);
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
                eprintln!("ERROR: Invalid input: unmatched quotes");
            }
            ReadCommandOutput::ReedlineError(err) => {
                eprintln!("ERROR: Input error: {}", err);
                break;
            }
            ReadCommandOutput::CtrlC => {
                println!("\nGoodbye!");
                break;
            }
            ReadCommandOutput::CtrlD => {
                println!("\nGoodbye!");
                break;
            }
        }
    }
    
    Ok(())
}

/// Handle a parsed REPL command
fn handle_command(command: ReplCommand, state: &mut RuntimeState) -> Result<(), RuntimeError> {
    match command {
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
                println!("No functions available.");
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
                println!("No tables available.");
            } else {
                println!("Available tables:");
                for table in tables {
                    println!("  - {}", table);
                }
            }
        }
        
        ReplCommand::Clear => {
            state.clear_data()?;
        }
        
        ReplCommand::Exit => {
            // This should be handled in the main loop, but just in case
            std::process::exit(0);
        }
    }
    
    Ok(())
}