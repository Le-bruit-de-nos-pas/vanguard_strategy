import os
import pandas as pd
from langchain_core.messages import HumanMessage, SystemMessage
from langchain_core.output_parsers import StrOutputParser
from langchain_community.vectorstores import Chroma
from langchain_huggingface import HuggingFaceEmbeddings
from langchain_community.llms import LlamaCpp
from langchain_community.document_loaders import PyPDFLoader

# Folder containing PDFs
PDF_FOLDER = "./papers"
RESULTS_CSV = "results.csv"

# Load local embedding model
embedding_model = HuggingFaceEmbeddings(model_name="intfloat/e5-large-v2")  # Stronger scientific model

# Define list of questions
questions = [
    "Describe the study population, including demographics like age, gender, and conditions. Return one detailed sentence or 'NOT APPLICABLE' if unknown.",
    "Summarize the study methods/interventions, including tools, design, and techniques used. Return one detailed sentence or 'NOT APPLICABLE' if unknown.",
    "Summarize the study findings, including main results and conclusions. Return one detailed sentence or 'NOT APPLICABLE' if unknown.",
]

# Load LLaMA model locally
llm = LlamaCpp(
    model_path="e5-mistral-7b-instruct-f16.gguf",
    temperature=0.3,  # More deterministic
    max_tokens=4096,  # Longer answers
    n_ctx=8192,  # Handles longer documents
)

def get_k_relevant_documents(documents, question, k=3):
    """Retrieve the top k relevant documents for the given question."""
    vector_store = Chroma.from_documents(documents, embedding_model)
    return vector_store.similarity_search(question, k=k)

def get_answer_from_llm(documents, question):
    """Get an answer from the LLM using retrieved documents as context."""
    relevant_docs = get_k_relevant_documents(documents, question)
    context_from_docs = "\n\n".join([doc.page_content for doc in relevant_docs])

    messages = [
        SystemMessage(content=f"""
        You are an expert summarizer for scientific papers.
        Use the following context to answer the question concisely with as much detail as possible.

        CONTEXT:
        {context_from_docs}

        IMPORTANT: If the question cannot be answered using the context, respond with 'NOT APPLICABLE.'
        """),
        HumanMessage(content=question),
    ]

    parser = StrOutputParser()
    chain = llm | parser
    return chain.invoke(messages)

def process_pdfs():
    """Process all PDFs in the folder and save results to CSV."""
    # Load existing results if the file exists
    if os.path.exists(RESULTS_CSV):
        df = pd.read_csv(RESULTS_CSV)
    else:
        df = pd.DataFrame(columns=["PDF Name", "Study Population", "Methods", "Findings"])

    processed_files = set(df["PDF Name"])  # Keep track of processed PDFs

    for pdf_file in os.listdir(PDF_FOLDER):
        if not pdf_file.endswith(".pdf"):
            continue  # Skip non-PDF files

        pdf_path = os.path.join(PDF_FOLDER, pdf_file)

        if pdf_file in processed_files:
            print(f"Skipping {pdf_file}, already processed.")
            continue  # Skip if already processed

        print(f"Processing: {pdf_file}")

        loader = PyPDFLoader(pdf_path)
        documents = list(loader.lazy_load())  # Load all pages

        answers = [get_answer_from_llm(documents, q) for q in questions]

        print(answers)
        # Append new result
        new_entry = pd.DataFrame([[pdf_file] + answers], columns=df.columns)
        df = pd.concat([df, new_entry], ignore_index=True)

        # Save updated CSV after each PDF
        df.to_csv(RESULTS_CSV, index=False)

        print(f"Finished processing {pdf_file} ✅\n")

    print("All PDFs processed! Results saved to:", RESULTS_CSV)

# Run the script
process_pdfs()
