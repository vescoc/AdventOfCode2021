use std::{fs, env};
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    Token,
    braced,
    Ident,
    Lit,
    LitStr,
    Error,
    parse::{
        Parse,
        ParseStream
    }
};

struct Alu {
    name: Ident,
    instrs: Vec<Instr>,
}

enum Instr {
    Inp(Ident),
    Add(Ident, VarOrLit),
    Mul(Ident, VarOrLit),
    Div(Ident, VarOrLit),
    Mod(Ident, VarOrLit),
    Eql(Ident, VarOrLit),
    Include(Instrs),
}

enum VarOrLit {
    Var(Ident),
    Lit(Lit),
}

struct Instrs(Vec<Instr>);

impl ToTokens for VarOrLit {
    fn to_tokens(&self, stream: &mut proc_macro2::TokenStream) {
        match self {
            VarOrLit::Var(id) => stream.extend(quote! { self.#id }),
            VarOrLit::Lit(value) => value.to_tokens(stream),
        }
    }
}

impl Parse for Alu {
    fn parse(stream: ParseStream) -> Result<Self, syn::Error> {
        let name = stream.parse::<Ident>()?;
        
        let content;
        braced!(content in stream);
        let instrs = content.parse::<Instrs>()?;
        
        Ok(Alu {
            name,
            instrs: instrs.0,
        })
    }
}

impl Parse for VarOrLit {
    fn parse(stream: ParseStream) -> Result<Self, syn::Error> {
        if stream.peek(Lit) {
            Ok(VarOrLit::Lit(stream.parse()?))
        } else {
            Ok(VarOrLit::Var(stream.parse()?))
        }
    }
}

impl Parse for Instrs {
    fn parse(stream: ParseStream) -> Result<Self, syn::Error> {
        let mut instrs = vec![];
        while !stream.is_empty() {
            instrs.push(stream.parse()?);
        }            
        Ok(Instrs(instrs))            
    }
}

impl Parse for Instr {
    fn parse(stream: ParseStream) -> Result<Self, syn::Error> {
        if stream.peek(Token![mod]) {
            stream.parse::<Token![mod]>().ok();
            Ok(Instr::Mod(stream.parse()?, stream.parse()?))
        } else {
            let ident = stream.parse::<Ident>()?;
            if ident == "inp" {
                Ok(Instr::Inp(stream.parse()?))
            } else if ident == "add" {
                Ok(Instr::Add(stream.parse()?, stream.parse()?))
            } else if ident == "mul" {
                Ok(Instr::Mul(stream.parse()?, stream.parse()?))
            } else if ident == "div" {
                Ok(Instr::Div(stream.parse()?, stream.parse()?))
            } else if ident == "eql" {
                Ok(Instr::Eql(stream.parse()?, stream.parse()?))
            } else if ident == "include" {
                let path = stream.parse::<LitStr>()?;
                let include = fs::read_to_string(&path.value())
                    .map_err(|e| Error::new(path.span(), format!("cwd: {:?} e: {:?}", env::current_dir().unwrap(), e)))?;
                Ok(Instr::Include(syn::parse_str(&include)?))
            } else {
                Err(Error::new(ident.span(), "Invalid instruction"))
            }
        }
    }
}

fn instrs_to_stmts(instrs: Vec<Instr>) -> impl Iterator<Item=proc_macro2::TokenStream> {
    instrs.into_iter().flat_map(|instr| match instr {
        Instr::Inp(id) => vec![quote! { self.#id = i.next()?; }],
        Instr::Add(id, value) => vec![quote! { self.#id += #value; }],
        Instr::Mul(id, value) => vec![quote! { self.#id *= #value; }],
        Instr::Div(id, value) => vec![quote! { self.#id /= #value; }],
        Instr::Mod(id, value) => vec![quote! { self.#id %= #value; }],
        Instr::Eql(id, value) => vec![quote! { self.#id = if self.#id == #value { 1 } else { 0 }; }],
        Instr::Include(instrs) => instrs_to_stmts(instrs.0).collect::<Vec<_>>(),
    })
}

#[proc_macro]
pub fn alu(input: TokenStream) -> TokenStream {
    let Alu { name, instrs } = syn::parse_macro_input!(input as Alu);

    let stmts = instrs_to_stmts(instrs);

    quote! {
        struct #name {
            w: i32,
            x: i32,
            y: i32,
            z: i32,
        }

        impl std::default::Default for #name {
            fn default() -> Self {
                #name::new()
            }
        }

        impl #name {
            fn new() -> Self {
                Self {
                    w: 0,
                    x: 0,
                    y: 0,
                    z: 0,
                }
            }
            
            fn run<I: std::iter::Iterator<Item=i32>>(&mut self, mut i: I) -> Option<i32> {
                #(#stmts)*
                
                Some(self.z)
            }
        }
    }.into()
}
