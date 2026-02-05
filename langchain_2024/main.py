import os
import json
import pdfplumber
import torch
from transformers import pipeline



# Path to folder with PDFs
PDF_FOLDER = "./papers"
OUTPUT_FOLDER = "./summaries"

# Load a summarization model
summarizer = pipeline("summarization", model="facebook/bart-large-cnn")

def extract_text(pdf_path):
    text = ""
    with pdfplumber.open(pdf_path) as pdf:
        for page in pdf.pages:
            text += page.extract_text() + "\n"
    return text

def summarize_text(text):
    instructions = "Please summarize the following text in a structured format with the key points and conclusions: \n1- Main Tools/methods Used (What they did?) \n2- Main Results/Findings (What they Observed/Concluded?) \n\n"
    text = instructions + text  # Add instructions at the beginning of the text
    
    
    chunks = [text[i:i+1024] for i in range(0, len(text), 1024)]  # Chunking for long texts
    summaries = [summarizer(chunk, max_length=50, min_length=0, do_sample=False, num_beams=4)[0]['summary_text'] for chunk in chunks]
    return " ".join(summaries)

def process_pdfs():
    os.makedirs(OUTPUT_FOLDER, exist_ok=True)
    
    for pdf in os.listdir(PDF_FOLDER):
        if pdf.endswith(".pdf"):
            pdf_path = os.path.join(PDF_FOLDER, pdf)
            text = extract_text(pdf_path)
            summary = summarize_text(text)
            
            # Save summary
            output_path = os.path.join(OUTPUT_FOLDER, pdf.replace(".pdf", ".json"))
            with open(output_path, "w") as f:
                json.dump({"file": pdf, "summary": summary}, f, indent=4)
            
            print(f"Processed: {pdf}")

if __name__ == "__main__":
    process_pdfs()
