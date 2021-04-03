# Gradient descent simple example with a network consisting of
# one neuron, one input, no bias, and activation function: f(x) = x.


# Train set. First element is input x and the second element is the output y.
train_set <- data.frame(x = c(3.0,4.0,1.0), y = c(1.5, 2.0, 0.5))

# Print the train set.
print(train_set)

# Forward propagation w*x
fp <- function(w, x){

    return(w * x)
}

# Loss function (y - y')^2
loss <- function(w, x, y){
  
  predicted <- fp(w, x) # This is y'
  
  return((y - predicted)^2)
}

# Derivative of the loss function.
derivative <- function(w, x, y){
  
  return(2.0 * x * ((x * w) - y))
}

# Gradient descent.
gradient.descent <- function(train_set, lr = 0.01, epochs = 5){
  
  w = -2.5 # Initialize weight at 'random'
  
  for(i in 1:epochs){
    
    derivative.sum <- 0.0
    
    loss.sum <- 0.0
    
    # Iterate each data point in train_set.
    for(j in 1:nrow(train_set)){
      point <- train_set[j, ]
      
      derivative.sum <- derivative.sum + derivative(w, point$x, point$y)
      
      loss.sum <- loss.sum + loss(w, point$x, point$y)
    }
    
    # Update weight.
    w <-  w - lr * derivative.sum
    
    # mean squared error (MSE)
    mse <- loss.sum / nrow(train_set)
    
    print(paste0("epoch: ", i, " loss: ",
                 formatC(mse, digits = 8, format = "f"),
                 " w = ", formatC(w, digits = 5, format = "f")))
  }
  
  return(w)
}

#### Train the 1 unit network with gradient descent ####
lr <- 0.01 # set learning rate.

set.seed(123)

# Run gradient decent to find the optimal weight.
learned_w = gradient.descent(train_set, lr, epochs = 10)

# Make predictions on new data using the learned weight.
fp(learned_w, 7)
