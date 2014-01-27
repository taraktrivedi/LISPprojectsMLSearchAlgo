% Autonomic Computing...Tarak Trivedi 11/2010
% Paper presentation Title: Automatically Generating Adaptive Logic to Balance
% Non-functional Tradeoffs During Reconfiguration
% Program: Implementation of genetic algorithm variant (Hermes) to verify the results.
% This genetic algorithm that would generate safe adaptation paths between starting and target configurations
% in a content distributed network.

% Clear workspace and close all windows
clear all;clc;close all;

%% Section for LUT

% Table: Look up Table for cost of instructions
% Note: Only three basic instructions used: Passivate, Link, Unlink

% Instructions Code       Cost
% Passivate    0           2
% Link         1           5
% Unlink       2           10

% Constructing LUT for the same
CostLUT = zeros(3,2);
CostLUT(1,:) = [0 5];
CostLUT(2,:) = [1 3];
CostLUT(3,:) = [2 2];

%% Section to configure overlay network topology

% Underlying network topology: <c1-c2> <c1-c3>...
% Starting configuration and Target configuration.
N = 5; % The number of nodes in the overlay network
MaxLinks = N*(N-1)/2;
% OverlayNetwork = struct('c1c2',-1, 'c1c3', -1,'c1c4',-1, 'c1c5', ...
%     -1,'c2c3',-1, 'c2c4', -1,'c2c5',-1, 'c3c4', -1,'c3c5',-1, 'c4c5', -1);
%StartConfig =struct(OverlayNetwork);
%TargetConfig =struct(OverlayNetwork);

% StartConfig = struct('c1c2',1, 'c1c3', -1,'c1c4',-1, 'c1c5', ...
%     -1,'c2c3',1, 'c2c4', 1,'c2c5',-1, 'c3c4', 1,'c3c5',-1, 'c4c5', 1);
% TargetConfig = struct('c1c2',1, 'c1c3', 1,'c1c4',1, 'c1c5', ...
%     1,'c2c3',-1, 'c2c4', -1,'c2c5',-1, 'c3c4', -1,'c3c5',-1, 'c4c5', -1);

% Alternative representation done automatically.
%Starting configuration and Target Configuration matrix representation
StartConfig = -1.*ones(N,N)+2.*eye(N);
TargetConfig = -1.*ones(N,N)+2.*eye(N);


StartConfig(1,2) = 1;
StartConfig(2,3) = 1;
StartConfig(2,4) = 1;
StartConfig(3,4) = 1;
StartConfig(4,5) = 1;

TargetConfig(1,2) = 1;
TargetConfig(1,3) = 1;
TargetConfig(1,4) = 1;
TargetConfig(1,5) = 1;

%% Section to setup initial configuration of GA

% GA parameters setup
MaxPop = 100; % Maximum Population size
CrType = 2;  % Crossover Type Two-Point
CrRate = 40; % Crossover Rate in Percentage
MutRate = 20; % Mutation Rate in Percentage
TSelect = 3; % Tournament Selection (K)
MaxGen = 1500; % Maximum Generations

% Initial setup for individuals and their encoding
% GA generations
EncoLen = 8;
gaVectEncoding = zeros(MaxPop,EncoLen);

% Initial set of candidate solutions or chromosome
% This is the processing vector
for i=1:MaxPop
encovect = randperm(300)-1;
gaVectEncoding(i,:) = encovect(:,1:EncoLen);
end

% Initialize the GA vectors
gaGen = zeros(MaxPop*2,EncoLen,MaxGen+1);

% Copy the parent generation in GA vector
gaGen(1:MaxPop,:,1) = gaVectEncoding;

% Initilaze fitness vectors
fitEval = zeros(MaxPop*2,EncoLen);

sindex = zeros(2*MaxPop,MaxGen);
topFit = zeros(MaxPop,MaxGen);
sindex1 = zeros(MaxPop,MaxGen);
topFit1 = zeros(MaxPop,MaxGen);
varB = zeros(2*MaxPop,MaxGen);

%% Start of the GA process: Crossover, Mutation,Migration operations
% i Specifies the ongoing generation in process

for i=1:MaxGen
    % GA Crossover operations
    % Set and choose random points for 2-site crossover principle
    Crs = randperm(EncoLen);
    Crspoints = sort(Crs(4:5));
    Crossovers = round((CrRate*MaxPop)/100); 
    q=randperm(MaxPop);
    qSelect = q(1:2*Crossovers);
        
        for k=1:2:length(qSelect)
          Parents = qSelect(k:k+1);
          a=Parents(1);
          b=Parents(2);
          Cross1= gaVectEncoding(a,:); 
          Cross2= gaVectEncoding(b,:);
          q1 = Cross1(Crspoints(1):Crspoints(2));
          q2 = Cross2(Crspoints(1):Crspoints(2));
          Cross1(Crspoints(1):Crspoints(2)) = q2;
          Cross2(Crspoints(1):Crspoints(2)) = q1;
          gaVectEncoding(a,:)=Cross1; 
          gaVectEncoding(b,:)=Cross2;
        end

    % Store the newly formed offsprings obtained from parents
    % End of crossover
    
    % GA Mutation operations...
    % Selection of random mutation for a candidate.
    Mutations = round((MutRate*MaxPop)/100); 
    mut=randperm(MaxPop);
    mSelect = mut(1:Mutations);
    linkToggle = round((EncoLen-1)*rand(1,Mutations)) + 1;
    
    for x=1:Mutations
        temp = randperm(300)-1;
        zChange = temp(19);
        gaVectEncoding(mSelect(x),linkToggle(x)) = zChange;
    end
    % New offsprings are stored
    % End of Mutation
    
    % 50% Migration from parent and child population to be included
    % in the mix
        gaGen(MaxPop+1:2*MaxPop,:,i) = gaVectEncoding;

 %% Find the fitness function here:
 % To evaluate the previous generation
% Initilize arrays which keep track of discarded solutions
    Sdiscard = zeros(1,MaxPop);
    costEval = zeros(2*MaxPop,MaxGen);
       
    for j=1:2*MaxPop
       
       fitEval(j,:) = gaGen(j,:,i);
       
       % Store individual digits in separate space
       storeDigits = zeros(EncoLen,3);
       for q=1:EncoLen
           
           num=fitEval(j,q);
        % 1st digit instruction code: 
        p0= floor(num/100); p0rem=mod(num,100);
        % 2nd digit component code:
        p1= floor(p0rem/10);
            if(p1==0 || p1==5)
            p1=5;
            else
                p1=abs(p1-5);
            end
         
        % 3rd digit component code 2: 
        p2 = mod(p0rem,10);
        
            if(p2==0 || p2==5)
            p2=5;
            else
                p2=abs(p2-5);
            end
        
            storeDigits(q,1)=p0;
            storeDigits(q,2)=p1;
            storeDigits(q,3)=p2;
       end
     
	% Initially assign nodes as not passivated to ensure safe path  
     u=struct('Node',{1 2 3 4 5},'passivated',{'No','No','No','No','No'});
      
     % Safe path condition and discard those solutions which violate
      for r=1:EncoLen
           if(storeDigits(r,1) == 0)
                u(storeDigits(r,2)).passivated = 'Yes';
                u(storeDigits(r,3)).passivated = 'Yes';
                costEval(j,i)=costEval(j,i)+2;
           end
           
           if((storeDigits(r,1) == 1) || (storeDigits(r,1) == 2))
               
                tf1=strcmp(u(storeDigits(r,2)).passivated,'Yes');
                tf2=strcmp(u(storeDigits(r,3)).passivated,'Yes');
                
                if(~(tf1 && tf2) )
                Sdiscard(j)=1;
                costEval(j,i)=-999;
                elseif ((tf1 && tf2)&& storeDigits(r,1) == 1)
                   costEval(j,i)=costEval(j,i)+5;
                   
                elseif ((tf1 && tf2)&& storeDigits(r,1) == 2)
                   costEval(j,i)=costEval(j,i)+10;
                end
           
                
           end
      end
     
    end
    
    %% Scan for best fitting
    [fit,sindex(:,i)] = sort(costEval(:,i),'ascend');
       topFit(1:MaxPop,i) = fit(1:MaxPop);
        for d=1:MaxPop
         gaGen(d,:,i+1)= gaGen(sindex(d,i),:,i);
        end
                % Final fitness reordering based on closeness to target
                % configuration
                finalFit = zeros(MaxPop,EncoLen);
                costFit =  zeros(MaxPop,MaxGen);
            
                for k=1:MaxPop
                finalFit(k,:)= gaGen(k,:,i+1);
                end
    for k=1:MaxPop
        % Store individual digits in separate space
        storeDigits = zeros(EncoLen,3);
       for q=1:EncoLen
           
           num=finalFit(k,q);
        % 1st digit instruction code: 
        p0= floor(num/100); p0rem=mod(num,100);
        % 2nd digit component code:
        p1= floor(p0rem/10);
            if(p1==0 || p1==5)
            p1=5;
            else
                p1=abs(p1-5);
            end
         
        % 3rd digit component code 2: 
        p2 = mod(p0rem,10);
        
            if(p2==0 || p2==5)
            p2=5;
            else
                p2=abs(p2-5);
            end
        
            storeDigits(q,1)=p0;
            storeDigits(q,2)=p1;
            storeDigits(q,3)=p2;
       end
       
       % tempConfig
       tempConfig = -1.*ones(N,N)+2.*eye(N);
       for r=1:EncoLen
                      
                if(storeDigits(r,1) == 1)
                [v1 v2] = sort([storeDigits(r,2) storeDigits(r,3)]);
                tempConfig(v1,v2)=1; tempConfig(v2,v1)=1; 
                end
                
                if (storeDigits(r,1) == 2)
                [u1 u2] = sort([storeDigits(r,2) storeDigits(r,3)]);
                tempConfig(u1,u2)=-1; tempConfig(u2,u1)=-1;
                end
                
       end
     % Fitness values higher for those that match   
       for g=1:N
           for h=1:N
           if (tempConfig(g,h)==TargetConfig(g,h))
               costFit(k,i) = costFit(k,i) + 100;
           end
           
           end
       end
       
    end
     % Instead of using tournament selection use built-in sort for simplicity
     
     [fit1,sindex1(:,i)] = sort(costFit(:,i),'descend');
       topFit1(1:MaxPop,i) = fit1(1:MaxPop);
     
    for d=1:MaxPop
         gaGen(d,:,i+1)= gaGen(sindex1(d,i),:,i+1);
    end        
     gaVectEncoding = gaGen(1:MaxPop,:,i+1);
     
end % generation end

%% Results
% Graph of cost vs generation
cFitness=zeros(MaxGen);
for w=1:MaxGen
    cFitness(w) = topFit1(w);
end

i=1:1:1500;
y= sort(topFit1(i));
plot(i,y); 
title('Progression of maximum fitness Vs Generation');
axis([0 1500 0 2500])
xlabel('Generation');
ylabel('Maximum Fitness');
