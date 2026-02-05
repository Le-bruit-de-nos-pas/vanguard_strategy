import os
from langchain_core.messages import HumanMessage, SystemMessage
from langchain_core.output_parsers import StrOutputParser
from langchain_community.vectorstores import Chroma
from langchain_huggingface import HuggingFaceEmbeddings
from langchain_community.llms import LlamaCpp  # Local LLM

# Load local embedding model
embedding_model = HuggingFaceEmbeddings(model_name="intfloat/e5-large-v2")

def get_k_relevant_documents(documents, question, k=3):
    print(f"Storing {len(documents)} into Vector Store.")
    
    # Store documents in ChromaDB (local vector database)
    vector_store = Chroma.from_documents(documents, embedding_model)
    
    print("Getting relevant documents from local vector store.")
    relevant_docs = vector_store.similarity_search(question, k=k)
    print(f"Retrieved similar documents: {len(relevant_docs)}")
    
    return relevant_docs

def get_answer_from_llm(documents, question):
    print(f"Question: {question}")
    
    relevant_docs = get_k_relevant_documents(documents, question)   
    context_from_docs = "\n\n".join([doc.page_content for doc in relevant_docs])

    # Load LLaMA model locally
    llm = LlamaCpp(
        model_path="e5-mistral-7b-instruct-f16.gguf",  # Replace with your model path
        temperature=0.5,
        max_tokens=4000,
        n_ctx=10000
    )

    messages = [
        SystemMessage(content=f"""
    You are an expert summarizer for scientific literature. 
    Use the following retrieved context to answer my question concisely but with as much detail as possible: 

    CONTEXT: 
    {context_from_docs}

    IMPORTANT: If the question cannot be answered using the provided context, respond with 'NOT APPLICABLE.'
    """),
        HumanMessage(content=question),
    ]

    parser = StrOutputParser()
    chain = llm | parser
    return chain.invoke(messages)
