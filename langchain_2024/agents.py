import os
from PyPDF2 import PdfReader
from autogen import AssistantAgent
import json
import logging



class ResearchAgents:
    def __init__(self, api_key):
        self.groq_api_key = api_key
        self.llm_config = {
            'config_list': [
                {'model': 'llama-3.3-70b-versatile', 'api_key': self.groq_api_key, 'api_type': "groq"}
            ]
        }

        # Summarizer Agent - Summarizes research papers
        self.summarizer_agent = AssistantAgent(
            name="summarizer_agent",
            system_message="Summarize the retrieved research papers and present concise summaries to the user. JUST GIVE THE RELEVANT SUMMARIES OF THE RESEARCH PAPER AND NOT YOUR THOUGHT PROCESS.",
            llm_config=self.llm_config,
            human_input_mode="NEVER",
            code_execution_config=False
        )

        # Advantages and Disadvantages Agent - Analyzes pros and cons
        self.advantages_disadvantages_agent = AssistantAgent(
            name="advantages_disadvantages_agent",
            system_message="Analyze the summaries of the research papers and provide a list of advantages and disadvantages for each paper in a pointwise format. JUST GIVE THE ADVANTAGES AND DISADVANTAGES, NOT YOUR THOUGHT PROCESS.",
            llm_config=self.llm_config,
            human_input_mode="NEVER",
            code_execution_config=False
        )

    def summarize_paper(self, paper_summary):
        """Generates a summary of the research paper."""
        try:
            response = self.summarizer_agent.generate_reply(
                messages=[{"role": "user", "content": f"Summarize this paper: {paper_summary}"}]
            )

            # Check if response is a dictionary and extract content
            if isinstance(response, dict):
                return response.get("content", "Summarization failed!")

            # If response is a string, try parsing JSON
            elif isinstance(response, str):
                try:
                    response_dict = json.loads(response.replace("'", "\""))  # Fix single quotes
                    return response_dict.get("content", "Summarization failed!")
                except json.JSONDecodeError:
                    logging.error("Failed to parse response as JSON.")
                    return "Summarization failed!"

            return "Unexpected response format."

        except Exception as e:
            logging.error(f"Error in summarizing paper: {str(e)}")
            return "Summarization failed due to an error."

    def analyze_advantages_disadvantages(self, summary):
        """Generates advantages and disadvantages of the research paper."""
        try:
            response = self.advantages_disadvantages_agent.generate_reply(
                messages=[{"role": "user", "content": f"Provide advantages and disadvantages for this paper: {summary}"}]
            )

            # Check if response is a dictionary and extract content
            if isinstance(response, dict):
                return response.get("content", "Analysis failed!")

            # If response is a string, try parsing JSON
            elif isinstance(response, str):
                try:
                    response_dict = json.loads(response.replace("'", "\""))  # Fix single quotes
                    return response_dict.get("content", "Analysis failed!")
                except json.JSONDecodeError:
                    logging.error("Failed to parse response as JSON.")
                    return "Analysis failed!"

            return "Unexpected response format."

        except Exception as e:
            logging.error(f"Error in analyzing advantages and disadvantages: {str(e)}")
            return "Analysis failed due to an error."
        




# --- Main function to read PDF, summarize, and analyze ---
def process_pdf(pdf_file_path):
    # Initialize ResearchAgents with the API key loaded from environment variables
    api_key = "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"
    if not api_key:
        raise ValueError("API key is missing. Make sure to set it in the .env file.")
    
    research_agents = ResearchAgents(api_key)

    # Read and extract text from PDF file
    pdf_text = read_pdf(pdf_file_path)

    # Summarize the research paper
    summary = research_agents.summarize_paper(pdf_text)
    print("Summary of the Paper:")
    print(summary)
    
    # Analyze the advantages and disadvantages
    analysis = research_agents.analyze_advantages_disadvantages(summary)
    print("\nAdvantages and Disadvantages:")
    print(analysis)



def read_pdf(pdf_file_path):
    """Reads the PDF file and extracts text."""
    try:
        reader = PdfReader(pdf_file_path)
        text = ""
        for page_num in range(len(reader.pages)):
            page = reader.pages[page_num]
            text += page.extract_text()
        
        return text
    except Exception as e:
        logging.error(f"Error reading the PDF file: {str(e)}")
        return "Error reading PDF."


# --- Execute the main process with your PDF ---
if __name__ == "__main__":
    # Specify your PDF file path here
    pdf_file_path = "papers/Visual preference for social stimuli in individuals with autism or neurodevelopmental disorders.pdf"  # Update this with your file path
    process_pdf(pdf_file_path)