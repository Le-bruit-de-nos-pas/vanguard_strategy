import PyPDF2
from transformers import pipeline, AutoModelForQuestionAnswering, AutoTokenizer
import pandas as pd
import re
from sumy.parsers.plaintext import PlaintextParser
from sumy.nlp.tokenizers import Tokenizer
from sumy.summarizers.text_rank import TextRankSummarizer


def summarize_pdf(pdf_path):
    """
    Extracts text from a PDF, summarizes it using TextRank from Sumy,
    and extracts key information.
    """

    # 1. Extract Text from PDF
    text = ""
    try:
        with open(pdf_path, 'rb') as file:
            reader = PyPDF2.PdfReader(file)
            for page_num in range(len(reader.pages)):
                page = reader.pages[page_num]
                text += page.extract_text()
    except FileNotFoundError:
        return "Error: PDF file not found."
    except Exception as e:
        return f"Error reading PDF: {e}"

    # 2. Use TextRank to reduce text size
    parser = PlaintextParser.from_string(text, Tokenizer("english"))
    summarizer = TextRankSummarizer()
    
    try:
        summary = summarizer(parser.document, 3)  # Summarize to 3 sentences (adjust as needed)
        reduced_text = ' '.join(str(sentence) for sentence in summary)
    except Exception as e:
        return f"Error during TextRank summarization: {e}"

    # 3. Summarize the reduced text using BART
    summarizer_bart = pipeline("summarization", model="sshleifer/distilbart-cnn-12-6")
    try:
        final_summary = summarizer_bart(reduced_text, max_length=1024, min_length=30, do_sample=False)[0]['summary_text']
    except Exception as e:
        return f"Error during BART summarization: {e}"

    # 4. Extract key information using Question Answering
    qa_model_name = "bert-large-uncased-whole-word-masking-finetuned-squad"
    qa_model = AutoModelForQuestionAnswering.from_pretrained(qa_model_name)
    qa_tokenizer = AutoTokenizer.from_pretrained(qa_model_name)
    qa_pipeline = pipeline("question-answering", model=qa_model, tokenizer=qa_tokenizer)

    def answer_question(question):
        try:
            result = qa_pipeline(question=question, context=reduced_text)
            return result['answer']
        except:
            return "Not found"

    doi = answer_question("What is the DOI of this input paper?")
    authors = answer_question("Who are the authors of this input paper?")
    title = answer_question("What is the title of this input paper?")
    methods_summary = answer_question("What methods were used in the study?")
    results_summary = answer_question("What are the main findings of the study?")
    conclusion_summary = answer_question("What is the conclusion of the study?")
    study_population = answer_question("What was the study population?")

    # 5. Create a Dictionary for the Table
    summary_data = {
        "Category": ["Paper DOI", "Author List", "Title of Paper", "Methods Summary",
                     "Findings/Results Summary", "Conclusion Summary", "Disease Area/Study Population"],
        "Description": [doi, authors, title, methods_summary, results_summary, conclusion_summary, study_population]
    }

    return summary_data

def create_table(summary_data):
    """
    Creates a Pandas DataFrame and formats it as a table.
    """
    df = pd.DataFrame(summary_data)
    return df.to_string(index=False)  # Returns a string representation of the table

# Main Execution
if __name__ == "__main__":
    pdf_file_path = "./papers/Accelerating eye movement research via accurate and affordable smartphone eye tracking.pdf"  # Replace with the actual path to your PDF
    summary_data = summarize_pdf(pdf_file_path)

    if isinstance(summary_data, str):
        print(summary_data)  # Print error message if any
    else:
        table = create_table(summary_data)
        print(table)
