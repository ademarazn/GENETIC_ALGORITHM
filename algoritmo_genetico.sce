clc;
clear;

// Função para gerar uma nova população
function [new_pop]=nova_populacao(nPop, nPrecInd)
    population = round(rand(nPop, nPrecInd));
    for i = 1:nPop do
        new_pop(i) = strcat(string(population(i,:)));
    end
endfunction

// Função para encontrar o fitness
function [fitness] = avaliafitness(pop)
    [nPop, nPrecInd] = size(pop);
    for i = 1:nPop do
        j=1;
        soma = 0.0;
        mult = 1.0;
        for k = 1:10 do
            xi = converter(part(pop(i),j:j+15));
            j=j+16;  
            soma = soma + xi^2 / 4000.0;
            mult = mult * cos(xi / sqrt(k));
        end
        fitness(i) = 1.0 + soma - mult;
    end
endfunction

// Função para converter de binário para decimal e
// colocar na escala de -5.12 a 5.12
function[valor] = converter(varbinario)
  valor = bin2dec(strcat(string(varbinario))); // recebe binário, transforma
  // para string, concatena todas as colunas do individuo,
  // e transforma para decimal
  aux = valor;
  precisao = 16;
  a = -5.12;
  b = 5.12;
  resultado = a + aux *(b-a) / 2^precisao - 1;
  valor = resultado;
endfunction

// Roda Roleta
// a) recebe população e fitness
function [new_pop]=roda_roleta(pop, fitness)
    // b) coloca em ordem crescente/decrescente o vetor fitness
    [fitness, old_pos] = gsort(fitness, "g", "d");
    
    // c) ordena a população conforme vetor fitness
    for i = 1:size(pop,1) do
        pop_asc(i) = pop(old_pos(i));
    end
    
    // d) inverter o fitness para que o menor número tenha maior possibilidade
    // de ser escolhido.
    soma=sum(fitness);
    for n=1:size(pop,1) do
        if fitness(n) == 0 then
            acumulado(n) = 0;
        else
            acumulado(n) = (fitness(n) / soma);
        end
    end

    soma=sum(acumulado);
    for n=1:size(pop,1) do
        if acumulado(n) ~= 0 then
            acumulado(n) = acumulado(n) / soma;
        end
    end
    
    // e) selecionar um número aleatório para encontrar um indivíduo para 
    // compor a nova população - realizar este passo n vezes,
    // onde n = nIndividuos.
    cs = cumsum(acumulado);
    new_pop = [];
    n = 1;
    while size(new_pop, 1) < size(pop, 1) do
        r = rand();
        for i=1:size(pop,1) do
            if cs(i) > r & size(new_pop, 1) < size(pop, 1) then
                new_pop(n) = pop_asc(i);
                n = n+1;
            end
        end
    end
endfunction

// Função para fazer cruzamento
function [Crossed_Indiv1, Crossed_Indiv2] = crossover(Indiv1,Indiv2)

    MultiCrossNb = 1;
    BinLen = length(Indiv1);

    // Crossover positions selection
    mix = unique(gsort(sample(MultiCrossNb, 1:BinLen-1), "g", "i"))';
    Crossed_Indiv1 = Indiv1;
    Crossed_Indiv2 = Indiv2;

    for j = 1:size(mix, "*") do
        H1 = part(Crossed_Indiv1, 1:mix(j)); //Head for Indiv1
        T1 = part(Crossed_Indiv1, (mix(j) + 1):BinLen); //Tail for Indiv1
        H2 = part(Crossed_Indiv2, 1:mix(j)); //Head for Indiv2
        T2 = part(Crossed_Indiv2, (mix(j) + 1):BinLen); //Tail for Indiv2

        Crossed_Indiv1 = [H1 + T2];
        Crossed_Indiv2 = [H2 + T1];
    end
endfunction

// Função para fazer mutação
function [Mut_Indiv1, Mut_Indiv2] = mutation(Indiv1, Indiv2)
    
    MultiMutNb = 1;
    dim = length(Indiv1);
    pos = grand(1, MultiMutNb, "uin", 1, dim);
    pos = unique(pos);
    Mut_Indiv1 = Indiv1;
    Mut_Indiv2 = Indiv2;
    for i = 1:size(pos, '*') do
        Mut_Indiv1 = [part(Mut_Indiv1, 1:pos(i) - 1), ..
        part(Mut_Indiv1, pos(i)), part(Mut_Indiv1, pos(i) + 1:dim)];
        if Mut_Indiv1(2) == "0" then
            Mut_Indiv1(2) = "1";
        else
            Mut_Indiv1(2) = "0";
        end
        Mut_Indiv1 = strcat(Mut_Indiv1);
        
        Mut_Indiv2 = [part(Mut_Indiv2, 1:pos(i) - 1), ..
        part(Mut_Indiv2, pos(i)), part(Mut_Indiv2, pos(i) + 1:dim)];
        if Mut_Indiv2(2) == "0" then
            Mut_Indiv2(2) = "1";
        else
            Mut_Indiv2(2) = "0";
        end
        Mut_Indiv2 = strcat(Mut_Indiv2);
    end
endfunction

// Gerar população inicial
nPop        = int(input('Digite a quantidade de indivíduos da população: '));
nPrecInd    = 160;
probCross   = double(input('Digite a probabilidade de crossover: '));
probMut     = double(input('Digite a probabilidade de mutação: '));
nVar        = 16;
nIter       = 10000;
bf          = ([]);
aux         = 1;

disp('Executando...');
tic(); //começa

pop = nova_populacao(nPop, nPrecInd);
// pop = [1,1,0,0,1,0;0,0,1,0,1,1;1,1,1,0,0,0;1,0,1,1,0,1];

if modulo(nPop, 2) ~= 0 then
    printf('\nO tamanho da população deve ser um número PAR!');
    abort;
end

// Verificando se nVar é divisor de nPrecInd
if modulo(nPrecInd, nVar) ~= 0 then
    printf('\nO valor de nVar deve ser divisor do ..
valor de nPrecInd que é %d', nVar, nPrecInd);
    abort;
end

// Avaliar a função objetivo
funcprot(0);
fitness = avaliafitness(pop);

// Seleção usando roda da roleta
pop = roda_roleta(pop, fitness);

nFob = nPop;
// LOOP PRINCIPAL DE OTIMIZAÇÃO
while nFob < nIter do
    // PARA cada membro da população
    for m = 1:2:nPop do
        // Escolher filho1 e filho2
        filho1 = pop(m);
        filho2 = pop(m+1);
        
        // Crossover
        pCross = rand();
        if pCross < probCross then
            [filho1, filho2] = crossover(strcat(string(filho1)), ..
            strcat(string(filho2)));
        end
        
        // Mutation usando os novos filho1 filho2
        pMut = pCross;
        if pMut < probMut then
            [filho1, filho2] = mutation(strcat(string(filho1)), ..
            strcat(string(filho2)));
        end
        
        // Add filhos na população no lugar dos selecionados
        pop(m)   = filho1;
        pop(m+1) = filho2;
    // FIM
    end

    // Avaliar fitness
    funcprot(0);
    fitness = avaliafitness(pop);
    
    // Seleção usando roda roleta
    pop = roda_roleta(pop, fitness);
    
    nFob = nFob + nPop;
    // add o melhor fitness (best fitness) na variável "bf"
    bf(aux, 1) = max(fitness);
    bf(aux, 2) = mean(fitness);
    bf(aux, 3) = min(fitness);
    aux = aux + 1;
// FIM LOOP
end
tempo = toc(); // termina

plot(bf(:,1), 'o-r');
plot(bf(:,2), 'o-g');
plot(bf(:,3), 'o-b');

title('Gráfico do Fitness');
xlabel('Fitness');
ylabel('Valor');
legend('Máximo', 'Médio', 'Mínimo');

printf('\nLevou %f segundo(s) para terminar', tempo);
printf('\n\nMenor fitness encontrado: %f', min(bf));

clear('i','m','filho1','filho2','fitness','pMut',..
'pCross','nFob','nIter','nVar');
