import os
import pandas as pd

def aggregate_excel_files(folder_path):
    # Crie um dicionário para armazenar os dados de cada dataset
    data_dict = {}

    # Percorra todos os arquivos na pasta especificada
    for filename in os.listdir(folder_path):
        if filename.endswith(".xlsx"):
            # Extraia o nome do dataset do nome do arquivo
            dataset_name = filename.split('-')[0]
            print(filename)

            # Leia as tabelas das sheets 'Classification Report' e 'Confusion Matrix'
            classification_report = pd.read_excel(os.path.join(folder_path, filename), sheet_name='Classification Report')
            confusion_matrix = pd.read_excel(os.path.join(folder_path, filename), sheet_name='Confusion Matrix')

            # Adicione o nome do arquivo como a primeira linha
            data = pd.concat([pd.DataFrame({filename: []}), classification_report, confusion_matrix], ignore_index=True)

            # Se o dataset já existe no dicionário, adicione os novos dados à direita dos existentes
            if dataset_name in data_dict:
                data_dict[dataset_name] = pd.concat([data_dict[dataset_name], data], axis=1)
            # Caso contrário, adicione os novos dados ao dicionário
            else:
                data_dict[dataset_name] = data

    # Crie um novo arquivo Excel para armazenar os dados agregados
    with pd.ExcelWriter('aggregated_data.xlsx') as writer:
        for dataset_name, data in data_dict.items():
            data.to_excel(writer, sheet_name=dataset_name, index=False)

# Substitua 'your_folder_path' pelo caminho da pasta onde estão os seus arquivos Excel
aggregate_excel_files(r'/Users/pt000063/Downloads/ML_v5/Proj-Dados/Teste_agg')
