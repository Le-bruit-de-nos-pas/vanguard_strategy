import openai
import os
from dotenv import load_dotenv


openai.api_key = os.getenv("OPENAI_API_KEY") 


print("API Key:", os.getenv("OPENAI_API_KEY"))


try:
    # Make a simple test request
    response = openai.ChatCompletion.create(
        model="gpt-4",
        messages=[{"role": "system", "content": "Say hello!"}]
    )
    print("API Key is valid! Response:", response["choices"][0]["message"]["content"])
except openai.OpenAIError as e:
    print("API Key is invalid:", e)