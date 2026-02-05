from langchain_ollama.llms import OllamaLLM
from langchain_community.document_loaders import PyPDFLoader  # Updated import for PyPDFLoader
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain.prompts import PromptTemplate
from langchain.schema.runnable import RunnableLambda
import pandas as pd
import logging
import json





class ScientificPaperSummarizer:
    def __init__(self, model_name="mistral"):
        """Initialize the summarizer with a local LLM model."""
        self.llm = OllamaLLM(model=model_name)  # Updated Ollama usage
        
        self.text_splitter = RecursiveCharacterTextSplitter(
            chunk_size=2000,
            chunk_overlap=200
        )
        
        # Define the summary template
        self.summary_template = """
        Analyze the following scientific paper excerpt and extract information for these categories:
        
        Text: {text}
        
        Please provide:
        1. Research Question/Objective
        2. Methodology
        3. Key Findings
        4. Limitations
        5. Future Research Directions
        
        Format your response as a Python dictionary with these exact keys:
        'research_question', 'methodology', 'key_findings', 'limitations', 'future_research'
        """
        
        self.prompt = PromptTemplate(
            input_variables=["text"],
            template=self.summary_template
        )
        
        # Updated to use RunnableSequence
        self.chain = self.prompt | self.llm
        


    def load_pdf(self, pdf_path):
        """Load and split PDF into chunks."""
        try:
            loader = PyPDFLoader(pdf_path)  # Updated PyPDFLoader import
            pages = loader.load()
            texts = self.text_splitter.split_documents(pages)
            return texts
        except Exception as e:
            logging.error(f"Error loading PDF: {str(e)}")
            raise
    


    def summarize_chunk(self, chunk):
        """Summarize a single chunk of text."""
        try:
            response = self.chain.invoke({"text": chunk.page_content})  # Get response
        
            #print("RAW RESPONSE:", response)  # Debugging
            print("Response Type:", type(response))  # Check the type

            # ✅ Case 1: If it's already a dictionary, return it
            if isinstance(response, dict):
                return response

            # ✅ Case 2: If it's a string, try parsing it as JSON
            if isinstance(response, str):
                try:
                    response_dict = json.loads(response.replace("'", "\""))  # Replace single quotes with double quotes
                    return response_dict
                except json.JSONDecodeError as e:
                    logging.error(f"JSON decoding failed: {e}")
                    return None

            logging.error("Unexpected response format.")
            return None

        except Exception as e:
            logging.error(f"Error summarizing chunk: {str(e)}")
            return None




    def summarize_chunk(self, chunk):
        """Summarize a single chunk of text."""
        try:
            response = self.chain.invoke({"text": chunk.page_content})  # Get response
        
            print("RAW RESPONSE:", response)  # Debugging step
            print("Response Type:", type(response))  # Check the type

            # Check if response is already a dictionary
            if isinstance(response, dict):
                return eval(response)  # No need to parse, return as is

            logging.error("Unexpected response format. Expected a dictionary.")
            return None

        except Exception as e:
            logging.error(f"Error summarizing chunk: {str(e)}")
            return None

  



    def merge_summaries(self, summaries):
        """Merge multiple chunk summaries into a final summary."""
        merged = {
            'research_question': [],
            'methodology': [],
            'key_findings': [],
            'limitations': [],
            'future_research': []
        }
        
        for summary in summaries:
            if summary:
                for key in merged.keys():
                    if summary.get(key):
                        merged[key].append(summary[key])
        
        # Create final summary by joining the lists
        final_summary = {
            key: ' '.join(set(value)) for key, value in merged.items()
        }
        
        return final_summary
    




    def summarize_paper(self, pdf_path, output_path=None):
        """Process entire paper and generate summary."""
        try:
            # Load and split the PDF
            chunks = self.load_pdf(pdf_path)
            
            # Summarize each chunk
            chunk_summaries = []
            for chunk in chunks:
                summary = self.summarize_chunk(chunk)
                if summary:
                    chunk_summaries.append(summary)
            
            # Merge summaries
            final_summary = self.merge_summaries(chunk_summaries)
            
            # Convert to DataFrame
            df = pd.DataFrame([final_summary])
            
            # Save to CSV if output path is provided
            if output_path:
                df.to_csv(output_path, index=False)
            
            return df
        
        except Exception as e:
            logging.error(f"Error processing paper: {str(e)}")
            raise


def main():
    # Example usage
    summarizer = ScientificPaperSummarizer()
    
    try:
        summary_df = summarizer.summarize_paper(
            pdf_path="papers/Visual preference for social stimuli in individuals with autism or neurodevelopmental disorders.pdf",
            output_path="summaries/summary.csv"
        )
        print("Summary generated successfully!")
        print("\nSummary Preview:")
        print(summary_df)
        
    except Exception as e:
        print(f"Error: {str(e)}")



if __name__ == "__main__":
    main()
