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
function [fitness]=func(pop, nVar, nPop, nPrecInd)
    //for i = 1:nPop do
        // x será o fitness do indivíduo 
     //   x = 0;
     //   for j = 1:nVar:nPrecInd do
     //       // disp(bin2dec(strcat(string(pop(i,j:j+1))))^2)
     //       x = x + (bin2dec(part(pop(i),j:j+nVar-1))^2);
     //   end
     //   fitnesses(i) = x;
    //end
    for n = 1:nPop do
        s = 0;
        p = 1;
        for i = 1:nVar:nPrecInd do
            // disp(bin2dec(strcat(string(pop(i,j:j+1))))^2)
            s = s+(bin2dec(part(pop(n),i:i+nVar-1))^2) / 4000;
            p = p*cos(bin2dec(part(pop(n),i:i+nVar-1)) / sqrt((i+nVar-1)/nVar));
        end
        fitness(n) = 1+s-p;
    end
endfunction

// Função para seleção pela Roda da Roleta
//function [new_pop]=roda_roleta(pop, fitness)
//    new_pop = [];
//    T = sum(fitness);
//    while size(new_pop,1) < size(pop,1) do
//        r = rand()*T;
//        S = 0;
//        for i = 1:size(pop,1) do
//            S = S + fitness(i);
//            if S >= r & size(new_pop,1) < size(pop,1) then
//                j = size(new_pop,1)+1;
//                new_pop(j) = pop(i);
//            end
//        end
//    end
//endfunction

// Roda Roleta
// a) recebe população e fitness
function [new_pop]=roda_roleta(pop, fitness)
    disp('antes');
    disp(fitness,pop);
    // b) coloca em ordem crescente/decrescente o vetor fitness
    [fitness, old_pos] = gsort(fitness, "g", "i");
    
    // c) ordena a população conforme vetor fitness
    for i = 1:size(pop,1) do
        pop_asc(i) = pop(old_pos(i));
    end
    disp('depois');
    disp(fitness, pop_asc);
    
    // d) inverter o fitness para que o menor número tenha maior possibilidade de ser escolhido.
    soma=sum(fitness);
    disp(soma);
    for n=1:size(pop,1) do
        if fitness(n) == 0 then
            acumulado(n) = 0;
        else
            acumulado(n) = 1 / (fitness(n) / soma);
        end
    end
    disp(acumulado);

    soma=sum(acumulado);
    disp(soma);
    for n=1:size(pop,1) do
        if acumulado(n) ~= 0 then
            acumulado(n) = acumulado(n) / soma;
        end
    end
    disp(acumulado);
    
    // e) selecionar um número aleatório para encontrar um indivíduo para compor a nova população - realizar este passo n vezes, onde n = nIndividuos.
    cs = cumsum(acumulado);
    disp(cs);
    new_pop = [];
    for n=1:size(pop,1) do
        r = rand();
        for i=1:size(pop,1) do
            // confirmar com o professor se usa > ou >=
            if cs(i) >= r then
                new_pop(n) = pop_asc(i);
            end
        end
    end
    disp(new_pop);
endfunction

// Função para fazer Crossover
function [Crossed_Indiv1, Crossed_Indiv2] = crossover(Indiv1,Indiv2)

    MultiCrossNb = 1;
    BinLen = length(Indiv1);

    // Crossover positions selection
    mix = unique(gsort(sample(MultiCrossNb, 1:BinLen-1), "g", "i"))';
    Crossed_Indiv1 = Indiv1;
    Crossed_Indiv2 = Indiv2;

    for j = 1:size(mix, "*")
        H1 = part(Crossed_Indiv1, 1:mix(j)); //Head for Indiv1
        T1 = part(Crossed_Indiv1, (mix(j) + 1):BinLen); //Tail for Indiv1
        H2 = part(Crossed_Indiv2, 1:mix(j)); //Head for Indiv2
        T2 = part(Crossed_Indiv2, (mix(j) + 1):BinLen); //Tail for Indiv2

        Crossed_Indiv1 = [H1 + T2];
        Crossed_Indiv2 = [H2 + T1];
    end
endfunction

// Função para fazer Mutation
function [Mut_Indiv1, Mut_Indiv2] = mutation(Indiv1, Indiv2)
    
    MultiMutNb = 1;
    dim = length(Indiv1);
    pos = grand(1, MultiMutNb, "uin", 1, dim);
    pos = unique(pos);
    Mut_Indiv1 = Indiv1;
    Mut_Indiv2 = Indiv2;
    for i = 1:size(pos, '*');
        Mut_Indiv1 = [part(Mut_Indiv1, 1:pos(i) - 1), ..
        part(Mut_Indiv1, pos(i)), part(Mut_Indiv1, pos(i) + 1:dim)];
        if Mut_Indiv1(2) == "0"
            Mut_Indiv1(2) = "1";
        else
            Mut_Indiv1(2) = "0";
        end
        Mut_Indiv1 = strcat(Mut_Indiv1);
        
        Mut_Indiv2 = [part(Mut_Indiv2, 1:pos(i) - 1), ..
        part(Mut_Indiv2, pos(i)), part(Mut_Indiv2, pos(i) + 1:dim)];
        if Mut_Indiv2(2) == "0"
            Mut_Indiv2(2) = "1";
        else
            Mut_Indiv2(2) = "0";
        end
        Mut_Indiv2 = strcat(Mut_Indiv2);
    end
endfunction



probCross   = 0.9;
probMut     = 0.7;
nVar        = 2;
nIter       = 10000;
bf          = [];

// Gerar população inicial
nPop        = int(input('Digite a quantidade de indivíduos da população: '));
nPrecInd    = int(input('Digite precisão/bits de cada indivíduo: '));
pop = nova_populacao(nPop, nPrecInd);
// pop = [1,1,0,0,1,0;0,0,1,0,1,1;1,1,1,0,0,0;1,0,1,1,0,1];

if modulo(nPop, 2) ~= 0 then
    printf('\nO tamanho da população deve ser um número PAR!');
    abort;
end

// Exibir a população inicial
disp(pop, 'Matriz populacional inicial');

// Verificando se nVar é divisor de nPrecInd
if modulo(nPrecInd, nVar) ~= 0 then
    printf('\nO valor de nVar deve ser divisor do ..
valor de nPrecInd que é %d', nVar, nPrecInd);
    abort;
end

// Avaliar a função objetivo
funcprot(0);
fitness = func(pop, nVar, nPop, nPrecInd);
//disp(fitness, 'Fitness dos indivíduos');

// Seleção usando roda da roleta
pop = roda_roleta(pop, fitness);

nFob = 0;
// LOOP PRINCIPAL DE OTIMIZAÇÃO
while nFob < nIter do
    // PARA cada membro da população
    for m = 1:2:nPop do
        // Escolher filho1 e filho2
        filho1 = pop(m);
        filho2 = pop(m+1);
        // disp(filho2,'filho2',filho1,'filho1');
        
        // Crossover
        pCross = rand();
        if pCross < probCross then
            [filho1, filho2] = crossover(strcat(string(filho1)), ..
            strcat(string(filho2)));
        end
        
        //disp(filho2, filho1);
        //disp('');
        
        // Mutation usando os novos filho1 filho2
        pMut = rand();
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
    fitness = func(pop, nVar, nPop, nPrecInd);

    // Seleção usando roda roleta
    pop = roda_roleta(pop, fitness);
    
    nFob = nFob + nPop;
    
    // add o melhor fitness (best fitness) na variável "bf"
    bf(nFob/nPop) = min(fitness);
// FIM LOOP
end

plot(bf);

clear('i','m','filho1','filho2','fitness','pMut',..
'pCross','nFob','nIter','nVar');
