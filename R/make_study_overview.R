library(tidyverse)

llm_sim_table <- tribble(
  ~reference, ~level_of_simulation, ~novelty, ~domain, ~result, ~prompt, ~LLM, ~note, ~preregistration,
  
  # 1. Aher et al.
  "Aher, G., Arriaga, R. I., & Kalai, A. T. (2023). Using Large Language Models to Simulate Multiple Humans and Replicate Human Subject Studies. https://doi.org/10.48550/arXiv.2208.10264",
  "individual",
  "replication",
  "ultimatum game, garden path sentences, risk aversion, Milgram shock experiments",
  "LLM simulated responses are 'largely consistent with prior human studies from the literature'",
  "names and title (simulating gender and ethnicity)",
  "GPT-3",
  "no clear outcome metric; relies on aggregate data of human studies",
  "no",
  
  # 2. Argyle et al.
  "Argyle et al. (2023). Out of One, Many: Using Language Models to Simulate Human Samples. Political Analysis. https://doi.org/10.1017/pan.2023.2",
  "aggregate (with subgroups)",
  "replication",
  "large-scale U.S. surveys; demographic subpopulations",
  "GPT-3 reproduces many subgroup distributions when conditioned; supports 'silicon samples'",
  "Conditioning on socio-demographic backstories",
  "GPT-3",
  "Promising subgroup emulation but biased and limited",
  "No",
  
  # 3. Bisbee et al.
  "Bisbee et al. (2024). Synthetic Replacements for Human Survey Data? Political Analysis. https://doi.org/10.1017/pan.2024.5",
  "aggregate + individual",
  "replication",
  "Public opinion surveys; sociopolitical attitudes",
  "LLMs approximate some aggregate patterns but distort prevalence and distributions",
  "Persona prompts",
  "ChatGPT",
  "Risks of naive synthetic replacement; distributional biases",
  "No",
  
  # 4. Chen, Hu & Lu (2025)
  "Chen, Hu & Lu (2025). Predicting Field Experiments with LLMs. https://doi.org/10.48550/arXiv.2504.01167",
  "aggregate experimental outcomes",
  "prediction (new data)",
  "319 field experiments (economics)",
  "Achieves ~78% accuracy; uneven across domains",
  "Automated prompting + ensemble models",
  "Multiple LLMs",
  "Accuracy skewed by topic; preprint",
  "No",
  
  # 5. Cui, Li & Zhou
  "Cui, Li & Zhou (n.d.). Can AI Replace Human Subjects? SSRN. https://doi.org/10.2139/ssrn.4940173",
  "participant & aggregate",
  "replication",
  "Large set of classical psychology experiments",
  "Mixed: some replications successful; high prompt sensitivity",
  "Standardized replication prompts",
  "Multiple LLMs",
  "Preprint; unclear metrics",
  "No",
  
  # 6. Doudkin et al.
  "Doudkin et al. (2025). AI persuading AI vs humans. https://doi.org/10.48550/arXiv.2503.02067",
  "participant response & persuasion effects",
  "prediction + comparison",
  "Pro-environmental persuasion experiments",
  "LLMs persuade LLMs differently than humans; effect sizes diverge",
  "Persuasion prompts",
  "GPT family",
  "Early-stage; unclear external validity",
  "No",
  
  # 7. Durmus et al.
  "Durmus et al. (2024). Measuring global subjective opinions in LLMs. https://doi.org/10.48550/arXiv.2306.16388",
  "aggregate (population-level)",
  "replication",
  "GlobalOpinionQA cross-national attitudes",
  "LLM opinions cluster toward U.S. views; conditioning helps but risks stereotyping",
  "Survey-style QA prompts",
  "Multiple models (Claude, GPT)",
  "Shows demographic skew; important limitation",
  "No",
  
  # 8. Guo et al.
  "Guo et al. (2025). Estimating Causal Effects of Text Interventions. https://doi.org/10.48550/arXiv.2410.21474",
  "causal-effect estimation",
  "prediction",
  "Text-treatment experiments",
  "LLMs can rank interventions but need calibration",
  "Counterfactual text generation",
  "Multiple LLMs",
  "Methodological; preprint",
  "No",
  
  # 9. Hecht et al. (2025)
  "Hecht et al. (2025). Using LLMs in Behavioral Science Interventions. Behavioral Science & Policy. https://doi.org/10.1177/23794607251344698",
  "conceptual",
  "N/A",
  "Intervention design, ethics, behavioral science",
  "LLMs promising but risky; guidance provided",
  "N/A",
  "N/A",
  "Perspective article",
  "No",
  
  # 10. Hewitt et al.
  "Hewitt et al. (2025). Predicting Results of Social Science Experiments. Preprint.",
  "aggregate & participant-level forecasting",
  "prediction",
  "Social science experiments",
  "Mixed performance; depends heavily on model & prompt",
  "Prediction prompts",
  "GPT family",
  "Preprint",
  "No",
  
  # 11. Hu et al. — SimBench
  "Hu et al. (2025). SimBench. https://doi.org/10.48550/arXiv.2510.17516",
  "benchmark (aggregate + individual)",
  "replication",
  "Human behavior benchmarks across tasks",
  "Highlights strengths and limits systematically",
  "Standardized benchmark prompts",
  "Multiple LLMs",
  "Benchmark still evolving",
  "No",
  
  # 12. Kaiser et al.
  "Kaiser et al. (2025). Personalized survey modeling with LLMs. https://doi.org/10.1145/3708319.3733685",
  "participant-level",
  "replication",
  "Survey-personalization & opinion modeling",
  "Shows potential but fairness/calibration problematic",
  "Persona conditioning",
  "LLMs (various)",
  "Exploratory conference paper",
  "No",
  
  # 13. Lippert et al. (2024)
  "Lippert et al. (2024). Predicting a complex behavioral science study. https://doi.org/10.1098/rsos.240682",
  "aggregate effect forecasting",
  "prediction (new data)",
  "Behavioral science experiment",
  "GPT-4 forecasts strongly correlate with observed effects",
  "Effect-size forecasting prompts",
  "GPT-4",
  "Single study; generalizability unclear",
  "No",
  
  # 14. Lukumon & Esenogho
  "Lukumon & Esenogho (2025). LLMs in the lab. https://doi.org/10.31219/osf.io/ucjnr_v1",
  "participant-level prediction",
  "prediction",
  "Laboratory experiments",
  "Mixed performance; fails on nuanced decision tasks",
  "Participant-simulation prompts",
  "GPT family",
  "Preprint",
  "No",
  
  # 15. Manning & Horton
  "Manning & Horton (2025). General social agents. https://doi.org/10.48550/arXiv.2508.17407",
  "conceptual",
  "N/A",
  "Social-agent theory",
  "Conceptual architecture; not empirical",
  "N/A",
  "N/A",
  "High-level conceptual",
  "No",
  
  # 16. Okada et al.
  "Okada et al. (2025). Behavior-change message effects. https://doi.org/10.1007/978-981-96-8298-0_29",
  "aggregate text-intervention effects",
  "prediction",
  "Behavior-change message effectiveness",
  "LLMs can rank messages reasonably well",
  "Message-effect prompts",
  "LLMs (various)",
  "Conference chapter; limited empirical depth",
  "No",
  
  # 17. Santurkar et al.
  "Santurkar et al. (2023). Whose opinions do LLMs reflect? https://doi.org/10.48550/arXiv.2303.17548",
  "aggregate",
  "replication",
  "Cross-national & demographic opinion distributions",
  "Models reflect some populations more than others",
  "Survey comparison prompts",
  "Multiple LLMs",
  "Important demographic-skew critique",
  "No",
  
  # 18. Schröder et al.
  "Schröder et al. (2025). LLMs do not simulate human psychology. https://doi.org/10.48550/arXiv.2508.06950",
  "participant & experimental tasks",
  "replication",
  "Psychological phenomena; cognitive tasks",
  "Systematic mismatches; critical perspective",
  "Replication prompts",
  "LLMs (various)",
  "Strong critique; argues fundamental limits",
  "No",
  
  # 19. Shrestha et al.
  "Shrestha et al. (2024). Beyond WEIRD synthetic survey participants. https://doi.org/10.1177/23794607241311793",
  "participant & aggregate",
  "replication",
  "Cross-national survey research",
  "LLMs reproduce some patterns but fail culturally specific items",
  "Persona conditioning",
  "GPT-3.5/4",
  "Shows limits for global policy research",
  "No",
  
  # 20. Sreedhar et al.
  "Sreedhar et al. (2025). IUI’25: user simulation for UI/UX. https://doi.org/10.1145/3708359.3712149",
  "participant simulation (HCI)",
  "prediction",
  "User interactions & UI/UX",
  "LLMs approximate some user behaviors",
  "Interaction-simulation prompts",
  "LLMs (various)",
  "Conference paper; limited generalizability",
  "No",
  
  # 21. Wang, Morgenstern & Dickerson
  "Wang, Morgenstern & Dickerson (2025). LLMs flatten identity groups. https://doi.org/10.48550/arXiv.2402.01908",
  "participant-level",
  "simulation critique",
  "Identity-sensitive survey responses",
  "LLMs flatten heterogeneity; harmful misrepresentation",
  "Analytic/simulation",
  "LLMs (various)",
  "Important fairness critique",
  "No",
  
  # 22. Yeykelis et al.
  "Yeykelis et al. (2025). Replication of 133 media-effects findings. https://doi.org/10.48550/arXiv.2408.16073",
  "aggregate experimental replication",
  "replication",
  "Media-effects experiments (133 findings)",
  "LLMs reproduce some effects but fail on others",
  "Replication prompts",
  "GPT family",
  "Large-scale; valuable but mixed",
  "No"
)


# quick inspect
llm_sim_table <- llm_sim_table %>% relocate(reference, year, level_of_prediction, novelty, main_result, method, sample_or_dataset, notes, url)
print(llm_sim_table)




