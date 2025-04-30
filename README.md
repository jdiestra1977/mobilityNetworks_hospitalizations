## Mobility Networks

In the directory `Data_from_TACC`, I have the node-level statistics of each weekly mobility network.  
Networks are at the **ZCTA level** for Texas, with weeks spanning from **2018 to February 2022**.  
This data is stored in the file `node_stats_TX.RData` (list).

### Example Entry

Each element of this list is a weekly network. Each row contains local statistics of each node in the network.

| ZCTA   | in_degree | out_degree | in_strength | out_strength | bet_cent    | clustering_local | Week       |
|--------|-----------|------------|-------------|--------------|-------------|------------------|------------|
| 75001  | 171       | 73         | 36490.5832  | 18788.0518   | 12514.33333 | 0.2645226        | 2022-02-28 |
| 75002  | 29        | 65         | 8377.8557   | 16069.6762   | 2274.83333  | 0.3794223        | 2022-02-28 |


## Correlation with influenza

I would need a similar list for hospitalizations in Texas. For each week between 2018 and February 2022:

- ZCTA
- Hospitalizations
- Week

## First test

I want to evaluate if there is a correlation between the position of the node (ZCTA) in the network, and epidemiological variables of interest.
Knowing that mobility in week *t* may influence hospitalizations in week *t+l*, I will evaluate different *l* values to find the optimal
*l* value. Epidemiological variables of interest include:

- **Total number of hospitalizations in season**. Total number of individuals hospitalized in each ZCTA at the end of the season.
- **Time of peak**. Number of weeks from the begginnig of the season when the prevalence curve reaches its maximum value.
- **Magnitud of peak**. Number of hospitalized individulals at the peak of the prevalence curve.
- **Onset of season**. Number of weeks from the begginning of the season when prevalence curves reach a pre-defined threshold *p*.
