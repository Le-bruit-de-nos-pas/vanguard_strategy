
import os
import streamlit as st
from dotenv import load_dotenv, find_dotenv
from langchain_experimental.agents.agent_toolkits.csv.base import create_csv_agent
from langchain_community.llms import OpenAI


def main():
 
    load_dotenv(find_dotenv())

    st.set_page_config(page_title="Enter CSV")
    st.header("Enter CSV")

    csv_file = st.file_uploader("Upload your CSV", type="csv")
    
    if csv_file is not None:

        agent = create_csv_agent(
            OpenAI(temperature=0), csv_file, verbose=True)

        user_question = st.text_input("Ask a question about your CSV: ")

        if user_question is not None and user_question != "":
            with st.spinner(text="In progress..."):
                st.write(agent.run(user_question))

if __name__ == "__main__":
    main()



