% create sankeys 
clear

% Crop type sankey (Fig. 7?)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% intensity --> croptype ---> region --->intensity 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% first step: create coded table in R
%%
sankeydata1 = readtable('../../output/figures/LUH2_GCB2025/sankeys/sankey_data/sankey_data_crops.xlsx', Sheet='data_sankey_change_2')
country_key = readtable('../../output/figures/LUH2_GCB2025/sankeys/sankey_data/sankey_data_crops.xlsx', Sheet='country_key')
country_key = sortrows(country_key,"lu_key","ascend")
croptype_key = readtable('../../output/figures/LUH2_GCB2025/sankeys/sankey_data/sankey_data_crops.xlsx', Sheet='ct_type_key')
croptype_key = sortrows(croptype_key,"lu_key","ascend");
intensity_key = readtable('../../output/figures/LUH2_GCB2025/sankeys/sankey_data/sankey_data_crops.xlsx', Sheet='intensity_key')
intensity_key = sortrows(intensity_key,"Intensity","ascend");


%%
% % Extract numeric arrays for clarity
% country     = sankeydata1{:,5};
% intensity   = sankeydata1{:,2};
% croptype    = sankeydata1{:,4};
% impact_change = sankeydata1{:,6}
idx = width(sankeydata1)-5
% Extract numeric arrays for clarity
country     = sankeydata1{:,4+idx};
intensity   = sankeydata1{:,1};
if any(intensity<1) 
    intensity = intensity+1
end
croptype    = sankeydata1{:,3+idx};
impact_change = sankeydata1{:,5+idx}
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
% 1D: countries -->10
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
country_group   = {'p','q','r','s','t','u','v','w','x','y'};

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
writetable(T, '../../output/figures/LUH2_GCB2025/sankeys/sankey_data/BD_int_croptype_reg_int_links.csv');
%% 
% create label table 

table_labels = [num2cell((1:sum([2*numel(intensity_names),2*numel(croptype_names),2*numel(country_names),2*numel(intensity_names)]))'), ...
                [repmat(intensity_names(:),2,1); repmat(croptype_names(:),2,1); repmat(country_names(:),2,1); repmat(intensity_names(:),2,1)], ...
                [repmat(intensity_group(:),2,1); repmat(croptype_group(:),2,1); repmat(country_group(:),2,1); repmat(intensity_group(:),2,1)]];

headers_name = {'no','name','group'}
Tname = cell2table(table_labels, 'VariableNames', headers_name);

writetable(Tname, '../../output/figures/LUH2_GCB2025/sankeys/sankey_data/BD_int_croptype_reg_int_names.csv');

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sankey Fig. 3 (intensity --> land use type --> region) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear
%%
%for intensity -->lutype -->region -->intensity 
sankeydata1 = readtable('../../output/figures/LUH2_GCB2025/sankeys/sankey_data/sankey_data_lu.xlsx', Sheet='data_sankey_change')
country_key = readtable('../../output/figures/LUH2_GCB2025/sankeys/sankey_data/sankey_data_lu.xlsx', Sheet='country_key')
country_key = sortrows(country_key,"country_group_key","ascend")
lu_key = readtable('../../output/figures/LUH2_GCB2025/sankeys/sankey_data/sankey_data_lu.xlsx', Sheet='lu_type_key')
lu_key = sortrows(lu_key,"lu_key","ascend");
intensity_key = readtable('../../output/figures/LUH2_GCB2025/sankeys/sankey_data/sankey_data_lu.xlsx', Sheet='intensity_key')
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
country_group   = {'i','j','k','l','m','n','o','p','q','r'};
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
writetable(T, '../../output/figures/LUH2_GCB2025/sankeys/sankey_data/BD_int_lu_reg_int_links.csv');
%% 
% create label table 
table_labels = [num2cell((1:sum([2*numel(intensity_names),2*numel(lu_names),2*numel(country_names),2*numel(intensity_names)]))'), ...
                [repmat(intensity_names(:),2,1); repmat(lu_names(:),2,1); repmat(country_names(:),2,1); repmat(intensity_names(:),2,1)], ...
                [repmat(intensity_group(:),2,1); repmat(lu_group(:),2,1); repmat(country_group(:),2,1); repmat(intensity_group(:),2,1)]];

headers_name = {'no','name','group'}
Tname = cell2table(table_labels, 'VariableNames', headers_name);

writetable(Tname, '../../output/figures/LUH2_GCB2025/sankeys/sankey_data/BD_int_lu_reg_int_names.csv');


