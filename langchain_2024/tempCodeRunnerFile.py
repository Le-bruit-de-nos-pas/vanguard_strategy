import os
from langchain_experimental.agents.agent_toolkits.csv.base import create_csv_agent
from langchain_community.llms import OpenAI

os.environ["OPENAI_API_KEY"] = ""

def list_csv_files():
    csv_files = []
    source_folder = "source"
    if os.path.exists(source_folder) and os.path.isdir(source_folder):
        for file_name in os.listdir(source_folder):
            if file_name.endswith(".csv"):
                csv_files.append(os.path.join(source_folder, file_name))
    return csv_files


def main():
    

    print("Welcome to CSV Analyzer!")
    csv_files = list_csv_files()

    if csv_files:
        print("Available CSV files:")
        for i, file_path in enumerate(csv_files, 1):
            print(f"{i}. {file_path}")

        while True:
            try:
                selection = int(input("Enter the number corresponding to the CSV file you want to analyze: "))
                if 1 <= selection <= len(csv_files):
                    csv_file_path = csv_files[selection - 1]
                    break
                else:
                    print("Invalid selection. Please enter a valid number.")
            except ValueError:
                print("Invalid input. Please enter a number.")

        agent = create_csv_agent(OpenAI(temperature=0), csv_file_path, verbose=True)

        while True:
            user_question = input("Ask a question about your CSV (type 'exit' to quit): ")

            if user_question.lower() == "exit":
                print("Exiting...")
                break
            
            answer = agent.run(user_question)
            print("Answer:", answer)
    
    else:
        print("No CSV files found in the 'source' folder.")

if __name__ == "__main__":
    main()


    