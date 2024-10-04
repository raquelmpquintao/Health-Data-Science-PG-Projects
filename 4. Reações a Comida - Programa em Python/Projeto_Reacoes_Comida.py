import matplotlib.pyplot as plt
import PySimpleGUI as sg

#Exercicio 1.1
def ler_dados_alergia(nome_ficheiro):
    alergias = []
    file = open(nome_ficheiro, 'r',encoding='utf-8') #fazer encoding para visualização de caracteres especiais
    lines = file.readlines()
    header=None  #inicia pelo else - permite linta de ingredientes seja mutavel (qualquer lista de ingredientes)
    for line in lines:
        if line.strip():
            if header != None:
                dados = line.strip().split(',')
                dados = dados[:-1] + dados[-1].split(':')

                linha_dicionario={}
                i=0 
                while i<len(header):
                    chave = header[i]
                    valor = dados[i]
                    if chave=='sujeito' or chave=='nascimento':
                        valor=int(valor)
                    elif chave=='genero':
                        valor=valor.strip()    #limpa o valor obtido
                    elif chave in ingredientes:
                        valor=int(valor)      #inteiro 
                    linha_dicionario[chave]=valor  #adiciona par chave:valor
                    i=i+1

                alergias.append(linha_dicionario)   #cria lista com cada dicionário para cada sujeito
            else:
                header_aux = line.strip().split(',')
                ingredientes_aux = header_aux[-1].split(':')
                header_aux = header_aux[:-1] + header_aux[-1].split(':')
                ingredientes=[]
                for ingrediente in ingredientes_aux:
                    ingredientes.append(ingrediente.strip())
                header=[]
                for coluna in header_aux:
                    header.append(coluna.strip())

    return alergias, ingredientes

#Exercicio 1.2
def ler_dados_receitas(nome_ficheiro):
    receitas = []
    file = open(nome_ficheiro, 'r', encoding='utf-8')
    lines = file.readlines()
    header = None 
    
    for line in lines:
        if line.strip():
            if header != None: 
                dados = line.strip().split(':')
                linha_dicionario={}
                i=0 
                while i<len(header):
                    chave = header[i]
                    valor = dados[i].strip()
                    if chave=='codigo' or chave=='código':
                        valor=int(valor)
                    elif i==i_lingredientes:
                        aux=dados[i:]
                        valor=[]
                        for ingrediente in aux:
                            if ingrediente.strip(): #verifica se está vazio (se n estiver vazio = TRUE)
                                valor.append(ingrediente.strip())
                        
                    linha_dicionario[chave]=valor  #adiciona par chave:valor
                    i=i+1
                receitas.append(linha_dicionario)
            else:
                header_aux = line.strip().split(':')
                header=[]
                for coluna in header_aux:
                    aux=coluna.strip()
                    if 'lista de ingredientes' in aux:
                        i_lingredientes = header_aux.index(coluna)
                        aux='lista de ingredientes' #coluna sem cacteres espciais '...'
                    if aux:   #so se aux tiver dados vai fazer append
                        header.append(aux)

    return receitas

#Exercicio 2
def max_alergias(contador_ingredientes):
    max_count = 0
    ingrediente_mais_alergico = None

    for ingrediente, count in contador_ingredientes.items():
        if count > max_count:
            max_count = count
            ingrediente_mais_alergico = ingrediente

    return ingrediente_mais_alergico

def ingrediente_com_mais_alergias(alergias,ingredientes):
    contador_ingredientes = {} #contar quantas pessoas alergicas para cada ingrediente

    for registo in alergias:
        for ingrediente in ingredientes:
            if registo[ingrediente] == 1:
                if ingrediente in contador_ingredientes.keys():
                    contador_ingredientes[ingrediente] += 1
                else:
                    contador_ingredientes[ingrediente] = 1
    
    ingrediente_mais_alergico = max_alergias(contador_ingredientes)
    #total_alergias = contador_ingredientes[ingrediente_mais_alergico] #devolver o nº de pessoas para cada ingrediente

    return ingrediente_mais_alergico #, total_alergias

#Exercicio 3
def ingrediente_com_mais_alergias_por_genero(alergias, ingredientes, genero):     
    contador_ingredientes = {}  # Inicia um dicionário vazio

# Verificar as alergias por registo de alergias (sujeito) e criar uma estrutura de dados para guardar o numero de alergicos por ingrediente,
# para depois comparar qual o ingrediente com o numero maior de alergicos o código torna-se mais eficiente do que verificar 
# ingrediente a ingrediente qual o numero de alergicos e guardando o ultimo com maior numero de alergicos, visto que existem menos ingredientes
# do que pessoas no registo de alergias.
    
    # Itera sobre os registros, filtrando por gênero e contando alergias por ingrediente
    for registo in alergias:
        if registo['genero'] == genero:
            for ingrediente in ingredientes:
                # Inicializa a contagem se o ingrediente ainda não foi adicionado ao dicionário
                if ingrediente not in contador_ingredientes:
                    contador_ingredientes[ingrediente] = 0
                
                # Verifica se o registro indica alergia ao ingrediente (1 para alergia)
                if registo.get(ingrediente, 0) == 1:
                    contador_ingredientes[ingrediente] += 1

    # Encontra o ingrediente com o maior número de alergias
    ingrediente_mais_alergico = None
    max_alergias = 0
    for ingrediente, count in contador_ingredientes.items():
        if count > max_alergias:
            max_alergias = count
            ingrediente_mais_alergico = ingrediente
    return ingrediente_mais_alergico#, max_alergias

#Exercicio 4
def contar_ingredientes_alergenicos_em_produto(alergias, id_receita): #função com dois parâmetros: alergias e produto.
    ingredientes_alergenicos = 0 #variável inicializada, serve para contar a quantidade de ingredientes alérgicos 
    for receita in receitas:
        if id_receita==receita['código']:
            ingredientes=receita['lista de ingredientes']
            break

    for ingrediente in ingredientes: #iniciar loop para iterar cada ingrediente em ingredientes.
        for registo in alergias: #Dentro deste loop, outro loop for é iniciado para iterar sobre cada alergia em alergias.
            if registo.get(ingrediente, 0) == 1: #if verifica se o ingrediente atual é um alérgico (ou seja, se existe em alergias com valor 1).
                ingredientes_alergenicos += 1 #Se True, ingredientes_alergenicos é incrementado em 1 e o loop interno é encerrado com break.
                break
    return ingredientes_alergenicos #a função retorna a contagem de ingredientes alérgicos.

#Exercicio 5
def n_pessoas_alergicas_a_receita(id_receita,receitas,alergias):
    
    for receita in receitas:
        if id_receita==receita['código']:
            ingredientes_da_receita=receita['lista de ingredientes']
            break
    
    n_pessoas_alergicas=0
    for registo in alergias: #Percorrer a lista de dicionarios com os registos de alergias para cada pessoa
        for ingrediente in ingredientes_da_receita: #Percorrer ingredientes da receita
            if ingrediente in registo.keys(): #Verificar se ingrediente está presente nas chaves do registo de alergias
                if registo[ingrediente]==1: #Verificar se esse ingrediente está assinalado como tendo alergia
                    n_pessoas_alergicas=n_pessoas_alergicas+1
                    break
    
    return n_pessoas_alergicas

#Exercicio 6
def receitas_sem_alergicos_por_genero(genero,receitas,alergias):
    #Reaproveitamento do codigo do exercicio 5 mas para todas as receitas. Assim sabemos a contagem de pessoas alergicas em todas as receitas
    receitas_com_zero=[] #lista para guardarmos as receitas com zero pessoas alergicas
    for receita in receitas: #Percorre todas as receitas
        
        ingredientes_da_receita=receita['lista de ingredientes']
        encontrou_alergia=False
        for registo in alergias: #Percorrer a lista de dicionarios com os registos de alergias para cada pessoa
            if registo['genero']==genero:
                for ingrediente in ingredientes_da_receita:  #Percorrer ingredientes da receita
                    if ingrediente in registo.keys(): #Verificar se ingrediente está presente nas chaves do registo de alergias
                        if registo[ingrediente]==1:
                            encontrou_alergia=True
                            break   #Faz break ao ultimo ciclo for (ciclo dos ingrediemtes da receita)
            if encontrou_alergia:
                break #Faz break ao segundo ciclo for (ciclo dos registos das alergias)
        if encontrou_alergia:
                continue #Faz continue (passa à proxima receita) do primeiro ciclo for (ciclo das receitas)
        receitas_com_zero.append(receita['código'])
    return receitas_com_zero

#Exercício 7 - Perigosidade
def ordenar_relatorio_perigosidade(alergias,receitas):
    perigosidade = {}
    
    for receita in receitas:
        total_pessoas_alergicas = n_pessoas_alergicas_a_receita(receita['código'],receitas,alergias)
        if perigosidade.get(total_pessoas_alergicas,None)!=None:
            perigosidade[total_pessoas_alergicas]=perigosidade[total_pessoas_alergicas]+[receita['nome']]
        else:
            perigosidade[total_pessoas_alergicas] = [receita['nome']]
    chaves_ordenadas=perigosidade.keys()

    chaves_ordenadas=list(chaves_ordenadas)
  
    chaves_ordenadas.sort()
 
    perigosidade_ordenada={}
    for chave in chaves_ordenadas:
        lista_nome_receitas=perigosidade[chave]
        for nome_receita in lista_nome_receitas:
            perigosidade_ordenada[nome_receita]=chave
    return perigosidade_ordenada


def escrever_relatorio_de_perigosidade(alergias, receitas):
    perigosidade_ordenada=ordenar_relatorio_perigosidade(alergias,receitas)        
    arquivo = open('perigo.txt', 'w', encoding='utf-8')
    for nome_produto, numero_alergicos in perigosidade_ordenada.items():
        arquivo.write("%s: %d\n" % (nome_produto, numero_alergicos))
    arquivo.close()

# Exercicio 8 - Gráfico de barras
def grafico_de_barras(alergias,receitas):
    perigosidade_ordenada=ordenar_relatorio_perigosidade(alergias,receitas)        
    nomes_produtos = list(perigosidade_ordenada.keys())
    numeros_alergicos = list(perigosidade_ordenada.values())

    plt.figure(figsize=(10, 6))
    plt.barh(nomes_produtos, numeros_alergicos, color='skyblue')
    plt.xlabel('Número de Alérgicos')
    plt.ylabel('Nome da Receita')
    plt.title('Perigosidade dos Produtos')
    plt.gca().invert_yaxis()
    plt.tight_layout()
    plt.savefig('grafico_de_barras.png')
    plt.show()

#Exercicio 9 - Interface Gráfica:
sg.theme('DarkAmber')
layout = [
    [sg.Text('Path para o ficheiro de alergias:'), sg.InputText(), sg.FileBrowse()],
    [sg.Text('Path para o ficheiro de receitas:'), sg.InputText(), sg.FileBrowse()],
    [sg.Button('Avançar')],
]

window = sg.Window('Escolher Ficheiros', layout)

while True:
    event, values = window.read()
    if event == sg.WIN_CLOSED:
        break

    if event == 'Avançar':
        extensoes = ['csv', 'txt']

        try:
            ficheiro_alergias = values[0]
            ficheiro_receitas = values[1]
            extensao_alergias = ficheiro_alergias.split('.')[-1]
            extensao_receitas = ficheiro_receitas.split('.')[-1]

            if extensao_alergias not in extensoes or extensao_receitas not in extensoes:
                raise Exception

            alergias, ingredientes = ler_dados_alergia(ficheiro_alergias)
            

            receitas= ler_dados_receitas(ficheiro_receitas)
            

        except Exception as e:
            print(e)
            sg.popup('Ficheiro/s inválido!')
            continue

        #Preparar a dicionario das receitas para mostrar nas dropdown lists
        #nomes_receitas = []
        #codigos_receitas = []
        receitas_dict={}

        for receita in receitas:
            receitas_dict[receita['código']]=receita['nome']


        #input(list(receitas_dict.items()))
        # Define o layout da nova janela
        layout_new_window = [
            [
            sg.Column([
                [sg.Text('Qual o ingrediente com mais casos de alergia'), sg.Button('>', key='Botao1')],
                [sg.Text('Qual o ingrediente com maior nº de alergénicos:')],
                [sg.Radio('Feminino', "genero1",key='G1F', default=True), sg.Radio('Masculino', "genero1", key='G1M'), sg.Button('>', key='Botao2')],
                [sg.Text('Para um produto da empresa, quantos dos seus ingredientes estão sinalizados com casos de alergia:')],
                [sg.Combo(list(receitas_dict.items()), default_value=list(receitas_dict.items())[0], key='combo1'), sg.Button('>', key='Botao3')],
                [sg.Text('Nº de pessoas do estudo que manifestaram alergia a pelo menos um dos ingredientes de um dado produto:')],
                [sg.Combo(list(receitas_dict.items()), default_value=list(receitas_dict.items())[0], key='combo2'), sg.Button('>', key='Botao4')],
                [sg.Text('Receitas sem alérgicos:')],
                [sg.Radio('Feminino', "genero2",key='G2F', default=True), sg.Radio('Masculino', "genero2",key='G2M'), sg.Button('>', key='Botao5')],
                [sg.Text('Gerar relatório de perigosidade'), sg.Button('Gerar', key='Botao6'), sg.Button('Gráfico', key='Botao7')],
            ]),
            sg.Column([
                [sg.Text('Resultados:')],
                [sg.Multiline(size=(50, 16), key='output', autoscroll=True)],
            ])
            ],
        ]
        # Cria a nova janela
        new_window = sg.Window('Processamento de dados', layout_new_window)

        # Loop de evento para a nova janela
        while True:
            event_new, values_new = new_window.read()
            if event_new == sg.WINDOW_CLOSED:
                break
            elif event_new == 'Botao1':
                resultado = ingrediente_com_mais_alergias(alergias,ingredientes)
                new_window['output'].print('O ingrediente com mais alergias é: %s.\n' % resultado)
            elif event_new == 'Botao2':
                if values_new['G1M']:
                    genero = 'M'
                elif values_new['G1F']:
                    genero = 'F'
                resultado = ingrediente_com_mais_alergias_por_genero(alergias, ingredientes, genero)
                new_window['output'].print('O ingrediente com maior nº de alérgicos para o genero %s é %s.\n' % (genero, resultado))
            elif event_new == 'Botao3':
                receita_selecionada = values_new['combo1']
                id_receita = receita_selecionada[0]
                resultado = contar_ingredientes_alergenicos_em_produto(alergias, id_receita)
                new_window['output'].print("A receita %s possui %d ingredientes alergénicos.\n" % (receita_selecionada, resultado))
            elif event_new == 'Botao4':
                receita_selecionada = values_new['combo2']
                id_receita = receita_selecionada[0]
                resultado = n_pessoas_alergicas_a_receita(id_receita,receitas,alergias)
                new_window['output'].print("O número total de pessoas com alergia à receita %s é: %d.\n" % (receita_selecionada, resultado))
            elif event_new == 'Botao5':
                if values_new['G2M']:
                    genero = 'M'
                elif values_new['G2F']:
                    genero = 'F'
                resultado = receitas_sem_alergicos_por_genero(genero,receitas,alergias)
                if len(resultado) == 0:
                    new_window['output'].print("Não existem receitas sem alérgicos para o género %s.\n" %  genero)
                else:
                    for id_receita in resultado:
                        new_window['output'].print("A receita %s não tem alérgicos para o género %s.\n" % (receitas_dict[id_receita], genero))
            elif event_new == 'Botao6':
                escrever_relatorio_de_perigosidade(alergias, receitas)
                new_window['output'].print('Relatório gerado!\n\nPor favor, consulte ficheiro.')
            elif event_new == 'Botao7':
                grafico_de_barras(alergias,receitas)


        # Fecha a nova janela
        new_window.close()

window.close()

#testes 
'''
alergias, ingredientes = ler_dados_alergia('dados_alergia_comida_A.txt')
print(ingredientes)
print(alergias[0:4])

print("\n\n\n")

receitas = ler_dados_receitas("dados_receitas.csv")
print(receitas[0])
print(list(receitas[0].keys())[2])
print(ingrediente_com_mais_alergias(alergias,ingredientes))

#Exercicio 3
# Para o género feminino
genero_especifico = 'F'
ingrediente, numero_alergicos = ingrediente_com_mais_alergias_por_genero(alergias, ingredientes, genero_especifico)
print("Para o gênero %s, o ingrediente com mais alérgicos é %s com %d casos." % (genero_especifico, ingrediente, numero_alergicos))

# Para o género masculino
genero_especifico = 'M'
ingrediente, numero_alergicos = ingrediente_com_mais_alergias_por_genero(alergias, ingredientes, genero_especifico)
print("Para o gênero %s, o ingrediente com mais alérgicos é %s com %d casos." % (genero_especifico, ingrediente, numero_alergicos))

#Exercicio 4
# Teste: Se quisermos verificar a quantidade de ingredientes alergénicos no segundo produto da lista de receitas
numero_ingredientes_alergenicos = contar_ingredientes_alergenicos_em_produto(alergias, 3)
print("A receita %s possui %d ingredientes alergénicos." % ('3', numero_ingredientes_alergenicos))

#E assim por diante, podemos repetir para quantos produtos desejarmos testar.

#Exercicio 5
print('Ex5 funcao n_pessoas_alergicas_a_receita')
print(n_pessoas_alergicas_a_receita(5,receitas,alergias))
print('\n')

#Exercicio 6
print('Ex 6 funcao receitas_sem_alergenicos_por_genero')
print(receitas_sem_alergicos_por_genero('M',receitas,alergias))

#Exercício 7 - Perigosidade
# Chamar a função para criar o relatório de perigosidade
lista_ordenada = escrever_relatorio_de_perigosidade(alergias, receitas)

# Exercicio 8 - Gráfico de barras
# Chamar a função para plotar o gráfico de barras usando a lista ordenada
grafico_de_barras(alergias, receitas)'''
