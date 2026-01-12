% create sankeys 
clear

% Crop type sankey (Fig. 7?)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% intensity --> croptype ---> region --->intensity 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% first step: create coded table in R
%%
sankeydata1 = readtable('../../output/figures/sankey_data/sankey_data_crops.xlsx', Sheet='data_sankey_change_2')
country_key = readtable('../../output/figures/sankey_data/sankey_data_crops.xlsx', Sheet='country_key')
country_key = sortrows(country_key,"lu_key","ascend")
croptype_key = readtable('../../output/figures/sankey_data/sankey_data_crops.xlsx', Sheet='ct_type_key')
croptype_key = sortrows(croptype_key,"lu_key","ascend");
intensity_key = readtable('../../output/figures/sankey_data/sankey_data_crops.xlsx', Sheet='intensity_key')
intensity_key = sortrows(intensity_key,"Intensity","ascend");

%%
% % Extract numeric arrays for clarity
% country     = sankeydata1{:,5};
% intensity   = sankeydata1{:,2};
% croptype    = sankeydata1{:,4};
% impact_change = sankeydata1{:,6}

% Extract numeric arrays for clarity
country     = sankeydata1{:,4};
intensity   = sankeydata1{:,1};
if any(intensity<1) 
    intensity = intensity+1
end
croptype    = sankeydata1{:,3};
impact_change = sankeydata1{:,5}
% Preallocate based on max indices
nCountry     = max(country);
nIntensity   = max(intensity);
nCropType    = max(croptype);
impact_change(isnan(impact_change)) = 0;
%%
BD_3D = zeros(nCountry, nIntensity, nCropType);
% Fill
for f = 1:height(sankeydata1)
    c  = country(f);
    i  = intensity(f);
    cr = croptype(f);
    BD_3D(c,i,cr) = BD_3D(c,i,cr) + impact_change(f);
end

%%
size(BD_3D)

% BD_4D_re
% 1D: countries -->11
% 2D: intensity (low, medium, high) -->3
% 3D: crop type --> 12 
% 4D: year -->20
%%

% check sum change
sum(sum(sum(BD_3D, "omitnan")))
sum(impact_change, "omitnan")
%%
% BD_change_3D
% 1D: countries -->175
% 2D: intensity (low, medium, high, abandoned) -->4
% 3D: land use type (plantations, crops, pasture, abandoned) -->4


% separate increases and decreases in impacts
BD_change_3D_agg_pos = max(BD_3D,0);
BD_change_3D_agg_neg = max(-1*BD_3D,0);

% compile total increases and decreases in impacts
sum_pos_neg(1,1) = sum(sum(sum(BD_change_3D_agg_pos)))
sum_pos_neg(2,1) = sum(sum(sum(BD_change_3D_agg_neg)))


%%
% compile linkages for sankey – intensity vs croptype
E_Int_Ct_pos(:,:) = sum(BD_change_3D_agg_pos,1);
E_Int_Ct_neg(:,:) = sum(BD_change_3D_agg_neg,1);

% compile linkages for sankey – crop type vs region
E_Preg_Ct_pos(:,:) = sum(BD_change_3D_agg_pos,2);
E_Preg_Ct_neg(:,:) = sum(BD_change_3D_agg_neg,2);
E_Ct_Preg_pos(:,:) = E_Preg_Ct_pos';
E_Ct_Preg_neg(:,:) = E_Preg_Ct_neg';

% compile linkages for sankey – region vs intensity
E_Preg_Int_pos(:,:) = sum(BD_change_3D_agg_pos,3);
E_Preg_Int_neg(:,:) = sum(BD_change_3D_agg_neg,3);


% compile data for sankey (created in R)

sankey_links(:,1) = [E_Int_Ct_pos(:);E_Int_Ct_neg(:);E_Ct_Preg_pos(:);E_Ct_Preg_neg(:);E_Preg_Int_pos(:);E_Preg_Int_neg(:)];
sankey_links_shares(:,1) = sankey_links(:,1) ./ (sum(sum(sum(BD_3D)))*-1);

% --- Create sankey table ---
%%
n_groups = ((nIntensity*2+nCropType+nCountry)*2)-1;
sankey_table_links(:,1) = (0:n_groups);
intensity_names = table2array(intensity_key(1:3,1)); % exclude abandoned land (not in this dataset, index is 4)
croptype_names = table2array(croptype_key(:,1));
country_names = table2array(country_key(:,1));

intensity_group = {'a','b','c'};
croptype_group  = {'d','e','f','g','h','i','j','k','l','m','n','o'};
country_group   = {'p','q','r','s','t','u','v','w','x','y','z'};

%% Create empty containers
rows = [];

%% 1️⃣ Intensity → Croptype links (pos + neg)
% --- Preallocate rows (columns: name, group, source, target, value, group2) ---
Nlinks = nIntensity*nCropType*2 + nCropType*nCountry*2;
rows = cell(Nlinks,4);
r = 1;

% --- 1️⃣ Intensity → Croptype (pos first, then neg) ---
for j = 1:nCropType

    % --- Positive links first ---
    tgt = nIntensity*2 + (j-1);   % croptype node index

    for i = 1:nIntensity
        src = i-1;  % intensity node index (0,1,2)
        rows(r,:) = { src, tgt, ...
                      E_Int_Ct_pos(i,j), intensity_group{i} };
        r = r + 1;
    end

    % --- Negative links second ---
    tgt = nIntensity*2+nCropType+(j-1)

    for i = 1:nIntensity
        src = i+nIntensity-1;  % 
        rows(r,:) = { src, tgt, ...
                      E_Int_Ct_neg(i,j), intensity_group{i} };
        r = r + 1;
    end
end



% --- 2️⃣ Croptype → Country (pos first, then neg) ---
for k = 1:nCountry
    

    % --- Positive links first ---
    tgt = nIntensity*2 + nCropType*2 + (k-1);  % country node index
    for j = 1:nCropType
        src = nIntensity*2 + (j-1);      % croptype node index
        rows(r,:) = { src, tgt, ...
                      E_Ct_Preg_pos(j,k), croptype_group{j} };
        r = r + 1;
    end

    % --- Negative links second ---
    tgt = nIntensity*2 + nCropType*2 + nCountry+ (k-1);  % country node index
    for j = 1:nCropType
        src = nIntensity*2+nCropType + (j-1);
        rows(r,:) = {  src, tgt, ...
                      E_Ct_Preg_neg(j,k), croptype_group{j} };
        r = r + 1;
    end
end

% --3 Country --> intensity (pos first, then neg) -- 
for i = 1:nIntensity
    tgt = nIntensity*2 + nCropType*2+nCountry*2+(i-1);  % intensity node index (0,1,2)

    % --- Positive links first ---
    for k = 1:nCountry
        src = nIntensity*2 + nCropType*2 + (k-1);  % country node index
        rows(r,:) = { src, tgt, ...
                      E_Preg_Int_pos(k,i), country_group{k} };
        r = r + 1;
    end

    % --- Negative links second ---
    tgt = nIntensity*2 + nCropType*2+nCountry*2+nIntensity+(i-1)    
    for k = 1:nCountry
        src = nIntensity*2 + nCropType*2+nCountry + (k-1);
        rows(r,:) = { src, tgt, ...
                      E_Preg_Int_neg(k,i), country_group{k} };
        r = r + 1;
    end
end


%% Convert to table
% write rows to disc
headers = {'source','target','value','group'};
T = cell2table(rows, 'VariableNames', headers);
writetable(T, '../../output/figures/sankey_data/BD_int_croptype_reg_int_links.csv');
%% 
% create label table 
table_labels = [num2cell((1:sum([2*numel(intensity_names),2*numel(croptype_names),2*numel(country_names),2*numel(intensity_names)]))'), ...
                [repmat(intensity_names(:),2,1); repmat(croptype_names(:),2,1); repmat(country_names(:),2,1); repmat(intensity_names(:),2,1)], ...
                [repmat(intensity_group(:),2,1); repmat(croptype_group(:),2,1); repmat(country_group(:),2,1); repmat(intensity_group(:),2,1)]];

headers_name = {'no','name','group'}
Tname = cell2table(table_labels, 'VariableNames', headers_name);

writetable(Tname, '../../output/figures/sankey_data/BD_int_croptype_reg_int_names.csv');

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sankey Fig. 3 (intensity --> land use type --> region) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear
%%
sankeydata1 = readtable('../../output/figures/sankey_data/sankey_data_lu.xlsx', Sheet='data_sankey_change')
country_key = readtable('../../output/figures/sankey_data/sankey_data_lu.xlsx', Sheet='country_key')
country_key = sortrows(country_key,"country_group_key","ascend")
lu_key = readtable('../../output/figures/sankey_data/sankey_data_lu.xlsx', Sheet='lu_type_key')
lu_key = sortrows(lu_key,"lu_key","ascend");
intensity_key = readtable('../../output/figures/sankey_data/sankey_data_lu.xlsx', Sheet='intensity_key')
intensity_key = sortrows(intensity_key,"Intensity","ascend");

%%
% Extract numeric arrays for clarity --> check in sankey data the column
% names
country     = sankeydata1{:,1};
intensity   = sankeydata1{:,2}; 
if any(intensity<1) 
    intensity = intensity+1
end 
lu_type    = sankeydata1{:,3};
impact_change = sankeydata1{:,4}

% Preallocate based on max indices
nCountry     = max(country);
nIntensity   = max(intensity);
nLuType    = max(lu_type);
impact_change(isnan(impact_change)) = 0;

%%
BD_3D = zeros(nCountry, nIntensity, nLuType);
% Fill
for f = 1:height(sankeydata1)
    c  = country(f);
    i  = intensity(f);
    lu = lu_type(f);
    BD_3D(c,i,lu) = BD_3D(c,i,lu) + impact_change(f);
end

%%
size(BD_3D)

%%

% check sum change
sum(sum(sum(BD_3D, "omitnan")))
sum(impact_change, "omitnan")
%%

% separate increases and decreases in impacts
BD_change_3D_agg_pos = max(BD_3D,0);
BD_change_3D_agg_neg = max(-1*BD_3D,0);

% compile total increases and decreases in impacts
sum_pos_neg(1,1) = sum(sum(sum(BD_change_3D_agg_pos)))
sum_pos_neg(2,1) = sum(sum(sum(BD_change_3D_agg_neg)))


%%
% compile linkages for sankey – intensity vs Country 
E_Int_Ct_pos(:,:) = sum(BD_change_3D_agg_pos,1);
E_Int_Ct_neg(:,:) = sum(BD_change_3D_agg_neg,1);

% compile linkages for sankey – lu vs country
E_Preg_Ct_pos(:,:) = sum(BD_change_3D_agg_pos,2);
E_Preg_Ct_neg(:,:) = sum(BD_change_3D_agg_neg,2);
E_Ct_Preg_pos(:,:) = E_Preg_Ct_pos';
E_Ct_Preg_neg(:,:) = E_Preg_Ct_neg';

% compile linkages for sankey – country vs intensity
E_Preg_Int_pos(:,:) = sum(BD_change_3D_agg_pos,3);
E_Preg_Int_neg(:,:) = sum(BD_change_3D_agg_neg,3);


% compile data for sankey (created in R)

sankey_links(:,1) = [E_Int_Ct_pos(:);E_Int_Ct_neg(:);E_Ct_Preg_pos(:);E_Ct_Preg_neg(:);E_Preg_Int_pos(:);E_Preg_Int_neg(:)];
sankey_links_shares(:,1) = sankey_links(:,1) ./ (sum(sum(sum(BD_3D)))*-1);

% --- Create sankey table ---
%%
n_groups = ((nIntensity*2+nLuType+nCountry)*2)-1;
sankey_table_links(:,1) = (0:n_groups);
intensity_names = table2array(intensity_key(1:4,1)); % exclude abandonment here 
lu_names = table2array(lu_key(:,1));
country_names = table2array(country_key(:,1));

intensity_group = {'a','b','c','d'};
lu_group  = {'e','f','g','h'};
country_group   = {'i','j','k','l','m','n','o','p','q','r','s'};
%%
%% Create empty containers
rows = [];

%% 1️⃣ Intensity → Croptype links (pos + neg)
% --- Preallocate rows (columns: name, group, source, target, value, group2) ---
Nlinks = nIntensity*nLuType*2 + nLuType*nCountry*2;
rows = cell(Nlinks,4);
r = 1;

% --- 1️⃣ Intensity → Croptype (pos first, then neg) ---
for j = 1:nLuType

    % --- Positive links first ---
    tgt = nIntensity*2 + (j-1);   % croptype node index

    for i = 1:nIntensity
        src = i-1;  % intensity node index (0,1,2)
        rows(r,:) = { src, tgt, ...
                      E_Int_Ct_pos(i,j), intensity_group{i} };
        r = r + 1;
    end

    % --- Negative links second ---
    tgt = nIntensity*2+nLuType+(j-1)

    for i = 1:nIntensity
        src = i+nIntensity-1;  % 
        rows(r,:) = { src, tgt, ...
                      E_Int_Ct_neg(i,j), intensity_group{i} };
        r = r + 1;
    end
end


% --- 2️⃣ Croptype → Country (pos first, then neg) ---
for k = 1:nCountry
    

    % --- Positive links first ---
    tgt = nIntensity*2 + nLuType*2 + (k-1);  % country node index
    for j = 1:nLuType
        src = nIntensity*2 + (j-1);      % croptype node index
        rows(r,:) = { src, tgt, ...
                      E_Ct_Preg_pos(j,k), lu_group{j} };
        r = r + 1;
    end

    % --- Negative links second ---
    tgt = nIntensity*2 + nLuType*2 + nCountry+ (k-1);  % country node index
    for j = 1:nLuType
        src = nIntensity*2+nLuType + (j-1);
        rows(r,:) = {  src, tgt, ...
                      E_Ct_Preg_neg(j,k), lu_group{j} };
        r = r + 1;
    end
end

% --3 Country --> intensity (pos first, then neg) -- 
for i = 1:nIntensity
    tgt = nIntensity*2 + nLuType*2+nCountry*2+(i-1);  % intensity node index (0,1,2)

    % --- Positive links first ---
    for k = 1:nCountry
        src = nIntensity*2 + nLuType*2 + (k-1);  % country node index
        rows(r,:) = { src, tgt, ...
                      E_Preg_Int_pos(k,i), country_group{k} };
        r = r + 1;
    end

    % --- Negative links second ---
    tgt = nIntensity*2 + nLuType*2+nCountry*2+nIntensity+(i-1)    
    for k = 1:nCountry
        src = nIntensity*2 + nLuType*2+nCountry + (k-1);
        rows(r,:) = { src, tgt, ...
                      E_Preg_Int_neg(k,i), country_group{k} };
        r = r + 1;
    end
end


%% Convert to table
% write rows to disc
headers = {'source','target','value','group'};
T = cell2table(rows, 'VariableNames', headers);
writetable(T, '../../output/figures/sankey_data/BD_int_lu_reg_int_links_2.csv');
%% 
% create label table 
table_labels = [num2cell((1:sum([2*numel(intensity_names),2*numel(lu_names),2*numel(country_names),2*numel(intensity_names)]))'), ...
                [repmat(intensity_names(:),2,1); repmat(lu_names(:),2,1); repmat(country_names(:),2,1); repmat(intensity_names(:),2,1)], ...
                [repmat(intensity_group(:),2,1); repmat(lu_group(:),2,1); repmat(country_group(:),2,1); repmat(intensity_group(:),2,1)]];

headers_name = {'no','name','group'}
Tname = cell2table(table_labels, 'VariableNames', headers_name);

writetable(Tname, '../../output/figures/sankey_data/BD_int_lu_reg_int_names_2.csv');


%%%% 
% Country - land use - sankey - needs some adjustments in excel
% Could be created as above, but replace croptype with land use type. 
%%%
%%
% clear BD_5D
% % year 2019
% for f = 1:562226;
%     BD_country = sankeydata1(f,1); 
%     BD_intensity = sankeydata1(f,2);
%     BD_landusetype = sankeydata1(f,3);
%     BD_croptype = sankeydata1(f,4);
%     BD_year = sankeydata1(f,7)-1999;
%     BD_5D(BD_country,BD_intensity,BD_landusetype,BD_croptype,BD_year) = sankeydata1(f,6);
% end
% 
% %%
% %size(BD_5D)
% 
% % aggregation of croptype
% BD_4D(:,:,2,:) = sum(BD_5D(:,:,2,1:143,:),4);
% BD_4D(:,:,4,:) = sum(BD_5D(:,:,4,1:143,:),4);
% BD_4D(:,:,[1,3],:) = sum(BD_5D(:,:,[1,3],144,:),4);
% 
% BD_4D_re = BD_4D(:,:,[4,2,3,1],:);
% size(BD_4D_re)
% 
% % BD_4D_re
% % 1D: countries -->175
% % 2D: intensity (low, medium, high, abandoned) -->4
% % 3D: land use type (plantations, crops, pasture, abandoned) -->4
% % 4D: year -->20
% 
% %change from 2000 to 2019
% BD_change_3D = BD_4D_re(:,:,:,20)-BD_4D_re(:,:,:,1);
% % check dimenstions
% size(BD_change_3D)
% % check sum change
% sum(sum(sum(BD_change_3D)))
% 
% % BD_change_3D
% % 1D: countries -->175
% % 2D: intensity (low, medium, high, abandoned) -->4
% % 3D: land use type (plantations, crops, pasture, abandoned) -->4
% 
% % aggregate regions:
% 
% brazil = 19;
% mexico = 98;
% otherlatinamerica = [5, 16, 29, 31, 42, 50, 64, 122, 123, 148, 167, 170];
% 
% indonesia = 70;
% china = 30;
% otherasiapacific = [1, 6, 7, 9, 10, 15, 20, 24, 35, 41, 51, 56, 69, 71, 72, 74, 78, 79, 80, 83, 84, 85, 87, 95, 100, 104, 106, 108, 109, 113, 115, 117, 118, 119, 121, 124, 128, 134, 139, 143, 146, 151, 152, 153, 155, 160, 161, 164, 168, 169, 171, 173];
% 
% drcongo = 37;
% tanzania = 154;
% otherafrica = [3, 4, 14, 18, 22, 23, 25, 27, 28, 39, 43, 45, 46, 48, 49, 54, 55, 58, 62, 63, 76, 81, 88, 89, 90, 93, 94, 96, 97, 102, 103, 105, 111, 112, 130, 133, 135, 136, 140, 141, 142, 144, 147, 157, 159, 162, 172, 174, 175];
% 
% europe = [2,8,11,12,17,21,33,36,38,47,52,53,57,59,67:68,73,75,82,86,91:92,99,101,107,114,116,125:126,129,131:132,137:138,145,149:150,163,165];
% northamerica = [13,26,32,34,40,44,60:61,65:66,77,110,120,127,156,158,166];
% 
% Agg_Preg = zeros(175,11);
% Agg_Preg(brazil,1) = 1;
% Agg_Preg(mexico,2) = 1;
% Agg_Preg(otherlatinamerica,3) = 1;
% Agg_Preg(indonesia,4) = 1;
% Agg_Preg(china,5) = 1;
% Agg_Preg(otherasiapacific,6) = 1;
% Agg_Preg(drcongo,7) = 1;
% Agg_Preg(tanzania,8) = 1;
% Agg_Preg(otherafrica,9) = 1;
% Agg_Preg(europe,10) = 1;
% Agg_Preg(northamerica,11) = 1;
% 
% 
% for l = 1:4
% BD_change_ind = BD_change_3D(:,:,l);
% BD_change_3D_agg(:,:,l) = Agg_Preg' * BD_change_ind;
% end
% 
% % check dimenstions
% size(BD_change_3D_agg)
% % check sum change
% sum(sum(sum(BD_change_3D_agg)))
% 
% %BD_change_3D_agg: 11 regions x 4 intensity levels x 4 land use types
% 
% 
% 
% %% Figure 2: 
% %Intensity
% %Land type
% %Region 
% %Intensity
% 
% % separate increases and decreases in impacts
% BD_change_3D_agg_pos = max(BD_change_3D_agg,0);
% BD_change_3D_agg_neg = max(-1*BD_change_3D_agg,0);
% 
% % compile total increases and decreases in impacts
% sum_pos_neg(1,1) = sum(sum(sum(BD_change_3D_agg_pos)))
% sum_pos_neg(2,1) = sum(sum(sum(BD_change_3D_agg_neg)))
% 
% 
% %%
% % compile linkages for sankey – intensity vs land use type
% E_Int_LU_pos(:,:) = sum(BD_change_3D_agg_pos,1);
% E_Int_LU_neg(:,:) = sum(BD_change_3D_agg_neg,1);
% 
% % compile linkages for sankey – land use type vs region
% E_Preg_LU_pos(:,:) = sum(BD_change_3D_agg_pos,2);
% E_Preg_LU_neg(:,:) = sum(BD_change_3D_agg_neg,2);
% E_LU_Preg_pos(:,:) = E_Preg_LU_pos';
% E_LU_Preg_neg(:,:) = E_Preg_LU_neg';
% 
% % compile linkages for sankey – region vs intensity
% E_Preg_Int_pos(:,:) = sum(BD_change_3D_agg_pos,3);
% E_Preg_Int_neg(:,:) = sum(BD_change_3D_agg_neg,3);
% 
% 
% % compile data for sankey (created in R)
% sankey_links(:,1) = [E_Int_LU_pos(:);E_Int_LU_neg(:);E_LU_Preg_pos(:);E_LU_Preg_neg(:);E_Preg_Int_pos(:);E_Preg_Int_neg(:)];
% sankey_links_shares(:,1) = sankey_links(:,1) ./ (sum(sum(sum(BD_change_3D_agg)))*-1);






