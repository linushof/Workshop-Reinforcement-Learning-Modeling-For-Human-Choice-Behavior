## Model with confirmatory and disconfirmatory alphas
This is a model which has two separate alpha parameters depending on whether the prediction error was positive or negative. It uses these alphas to update both chosen and unchosen cards. 

Read ["The computational roots of positivity and
confirmation biases in reinforcement learning"](https://www.sciencedirect.com/science/article/pii/S1364661322000894) 
 for more information.

### Parameter recovery
![Blue dashed lines indicate the true population parameter, black dot the posterior median, and black lines the 89% and 97% CI](parameter_recovery.png)
