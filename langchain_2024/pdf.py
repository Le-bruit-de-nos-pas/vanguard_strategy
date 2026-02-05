from langchain_community.document_loaders import PyPDFLoader
from get_relevant_documents import get_answer_from_llm

pdf_file_path = "./papers/Accelerating eye movement research via accurate and affordable smartphone eye tracking.pdf"

loader = PyPDFLoader(pdf_file_path)

documents = list(loader.lazy_load())  # Load all pages into a list

# List of questions to ask
questions = [
    "Please provide a detailed description of the study population, including any relevant demographic information such as age, gender, and specific conditions or disorders included. Return the answer as 1 single sentence but with as much detail as you can possibly retrieve! If you believe this questions cannot be answer or does not apply to this paper, answer NOT APPLICABLE.",
    "Please provide a detailed description of the study methods/interventions, including tools used, study design. Return the answer as 1 single sentence but with as much detail as you can possibly retrieve! If you believe this questions cannot be answer or does not apply to this paper, answer NOT APPLICABLE.",
    "Please provide a detailed description of the study findings, including the main finding/conlusion and any addiitonal key observations? Return the answer as 1 single sentence but with as much detail as you can possibly retrieve! If you believe this questions cannot be answer or does not apply to this paper, answer NOT APPLICABLE.",
    "Please provide a list of the authors names (for this paper!) Return the answer as 1 single sentence but with as much detail as you can possibly retrieve! If you believe this questions cannot be answer or does not apply to this paper, answer NOT APPLICABLE.",

]

# List to store answers
answers = []

# Loop through each question and store the answer
for question in questions:
    answer = get_answer_from_llm(documents=documents, question=question)
    answers.append(answer)

print("---------------FINAL ANSWERS-----------------------")
print(answers)