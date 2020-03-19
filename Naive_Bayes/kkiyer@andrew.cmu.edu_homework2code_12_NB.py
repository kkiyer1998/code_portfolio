import matplotlib
import numpy as np

def NB_XGivenY(XTrain, yTrain):
    # Implement your function here
    XTrainl = np.ndarray.tolist(XTrain)
    D = [[None]*len(XTrainl[0]),[None]*len(XTrainl[0])]
    beta1 = 1.001
    beta2 = 1.9
    transpose = np.ndarray.tolist(np.transpose(XTrain))

    # Iterating through each feature, and for each feature,
    # I'm interested when X=1 | y=1 and X=1 | y=2
    # I still need X=0 | y=1 and X=0 | y=2 to get the probabilities
    for i in range(len(transpose)):
        Xi = transpose[i]
        # alpha (0,1) for y = 1 
        alpha11 = 0
        alpha01 = 0
        # alpha (0,1) for y = 2
        alpha12 = 0
        alpha02 = 0
        for j in range(len(Xi)):
            if Xi[j] == 1 and yTrain[j] == 1:
                alpha11 = alpha11 + 1
            elif Xi[j] == 0 and yTrain[j] == 1:
                alpha01 = alpha01 + 1
            elif Xi[j] == 1 and yTrain[j] == 2:
                alpha12 = alpha12 + 1
            elif Xi[j] == 0 and yTrain[j] == 2:
                alpha02 = alpha02 + 1
        D[0][i] = (alpha11+beta1-1)/(alpha11+beta1-1+alpha01+beta2-1)
        D[1][i] = (alpha12+beta1-1)/(alpha12+beta1-1+alpha02+beta2-1)
    
        if(i==0):
            print(alpha01+alpha11)
            print(len(Xi))
            print(len(transpose))

    D = np.array(D)
    clipD = np.clip(D,10**-5,1-10**-5)
    return clipD


def NB_YPrior(yTrain):
    # Implement your function here
    num = 0.0
    den = 0.0
    for i in yTrain:
        if i == 1:
            num = num + 1
            den = den + 1
        else:
            den = den + 1
    return num/den
    pass

def NB_Classify(D, p, X):
    # Implement your function here
    yHat = np.zeros((1,len(X)),dtype=float)
    Yeq1 = p
    Yeq2 = 1-p
    X1 = np.ndarray.tolist(X)
    for x in range(len(X1)):
        Xnew = X1[x]
        for i in range(len(Xnew)):
            if Xnew[i] == 1:
                Yeq1 = Yeq1 + np.log(D[0,i])
                Yeq2 = Yeq2 + np.log(D[1,i])
            else:
                Yeq1 = Yeq1 + np.log(1-D[0,i])
                Yeq2 = Yeq2 + np.log(1-D[1,i])
        if(Yeq1>Yeq2):
            yHat[0,x] = 1
        else:
            yHat[0,x] = 2
    return yHat
    pass

def ClassificationError(yHat, yTruth):
    return 1 - np.mean(yHat == yTruth)

if __name__ == "__main__":

    import pickle
    with open("hw2data.pkl", "rb") as f:
        data = pickle.load(f)

    # You may want to convert XTrain/XTest from a sparse matrix to a dense matrix
    XTrain = data["XTrain"].todense()
    yTrain = data["yTrain"]

    XTest = data["XTest"].todense()
    yTest = data["yTest"]

    vocab = data["Vocabulary"]
    # Test your code here
    D = NB_XGivenY (XTrain,yTrain)
    p = NB_YPrior(yTrain)

    yHatTrain = NB_Classify(D,p,XTrain)
    yHatTest = NB_Classify(D,p,XTest)

    trainError = ClassificationError(yHatTrain,yTrain)
    testError = ClassificationError(yHatTest,yTest)

    print(trainError, testError)


    


