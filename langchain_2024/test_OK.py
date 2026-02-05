import pdfplumber
from transformers import BartTokenizer, BartForConditionalGeneration

tokenizer = BartTokenizer.from_pretrained("facebook/bart-large-cnn")
model = BartForConditionalGeneration.from_pretrained("facebook/bart-large-cnn")


def extract_text(pdf_path):
    text = ""
    with pdfplumber.open(pdf_path) as pdf:
        for page in pdf.pages:
            text += page.extract_text() + "\n"
    return text
pdf_path = "./papers/Accelerating eye movement research via accurate and affordable smartphone eye tracking.pdf"

cleaned_text = extract_text(pdf_path)

def summarize_text(text, model, tokenizer, max_chunk_size=1024):
    chunks = [text[i:i+max_chunk_size] for i in range(0, len(text), max_chunk_size)]
    summaries = []
    for chunk in chunks:
        inputs = tokenizer(chunk, max_length=max_chunk_size, return_tensors="pt", truncation=True)
        summary_ids = model.generate(
            inputs["input_ids"],
            max_length=200,
            min_length=50,
            length_penalty=2.0,
            num_beams=4,
            early_stopping=True
        )
        summaries.append(tokenizer.decode(summary_ids[0], skip_special_tokens=True))
    return " ".join(summaries)



def hierarchical_summarization(text, model, tokenizer, max_chunk_size=1024):
    first_level_summary = summarize_text(text, model, tokenizer, max_chunk_size)
   
    inputs = tokenizer(first_level_summary, max_length=max_chunk_size, return_tensors="pt", truncation=True)
    summary_ids = model.generate(
        inputs["input_ids"],
        max_length=200,
        min_length=50,
        length_penalty=2.0,
        num_beams=4,
        early_stopping=True
    )
    final_summary = tokenizer.decode(summary_ids[0], skip_special_tokens=True)
   
    return final_summary

final_summary = hierarchical_summarization(cleaned_text, model, tokenizer)

print(final_summary)