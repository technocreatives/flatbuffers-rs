use fbs_codegen::Parser;
use fbs_parser::ident;
use std::borrow::Cow;

fn run_data() -> Result<(), Box<dyn std::error::Error>> {
    let schema = fbs_parser::parse_path("./data.fbs")?;
    let schema = schema.last().unwrap();
    log::trace!("{:#?}", &schema);

    let file = std::fs::File::open("./data.bin")?;
    let mmap = unsafe { memmap::Mmap::map(&file)? };

    let parser = Parser::new(Cow::Borrowed(schema), &mmap[..])?;
    let data = parser.parse()?.try_into_table().unwrap();
    let fields = data.get(&ident("fields")?)?.unwrap().try_into_union_vector().unwrap();
    
    println!("{:#?}", fields);
    Ok(())
}

fn run_data2() -> Result<(), Box<dyn std::error::Error>> {
    let schema = fbs_parser::parse_path("./data2.fbs")?;
    let schema = schema.last().unwrap();
    log::trace!("{:#?}", &schema);

    let file = std::fs::File::open("./data2.bin")?;
    let mmap = unsafe { memmap::Mmap::map(&file)? };

    let parser = Parser::new(Cow::Borrowed(schema), &mmap[..])?;
    let data = parser.parse()?.try_into_table().unwrap();
    
    let an_int = data.get(&ident("an_int")?)?;
    println!("{:?}", an_int);

    let text = data.get(&ident("text")?)?;
    println!("{:?}", text);
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::init();

    log::info!("running Data2");
    run_data2()?;

    log::info!("running Data");
    run_data()?;

    Ok(())
}
