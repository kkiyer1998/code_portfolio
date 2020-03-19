from __future__ import division
from collections import OrderedDict
from commons import Variable
import numpy as np

dtype = np.float32


class DenseLayer(object):
    """A fully connected dense layer

    Parameters:
        w: in_dim x out_dim: The weight matrix
        n: out_dim, : the bias

    Arguments:
        n_out: number of output features
        init_type: the type of initialization
            can be gaussian or uniform

    """

    def __init__(self, n_out, init_type):
        self.n_out = n_out
        self.init_type = init_type
        self.params = OrderedDict()
        self.params["w"] = Variable()
        self.params["b"] = Variable()

    def get_output_dim(self):
        # The output dimension
        return self.n_out

    def init(self, n_in):
        # initializing the network, given input dimension
        scale = np.sqrt(1. / (n_in))
        if self.init_type == "gaussian":
            self.params["w"].value = scale * np.random.normal(
                0, 1, (n_in, self.n_out)).astype(dtype)
        elif self.init_type == "uniform":
            self.params["w"].value = 2 * scale * np.random.rand(
                n_in, self.n_out).astype(dtype) - scale
        else:
            raise NotImplementedError("{0} init type not found".format(
                self.init_type))
        self.params["b"].value = np.zeros((self.n_out), dtype=dtype)

    def forward(self, inputs):
        """The forward pass

        Arguments:
            inputs (``OrderedDict``): A dictionary containing
                height: the height of the current input
                width: the width of the current input
                channels: number of channels in the current
                    inputs
                data: a flattened data array n_in dimensions

        Returns:
            outputs (``OrderedDict``): A dictionary containing
                height: The height of the output (1 for a dense layer)
                width: The width of the output (1 for a dense layer)
                out_channels: The output number of feature maps
                    Same as the input channels for this layer
                data: a flattened output data array of the form
                    n_out dimensions

        """
        data = inputs["data"]
        outputs = OrderedDict()
        # cache for backward pass
        self.data = data
        outputs = OrderedDict()
        outputs["height"] = 1
        outputs["width"] = 1
        outputs["channels"] = self.n_out
        outputs["data"] = np.zeros((len(inputs["data"]), 1 * 1 * self.n_out), dtype=dtype)

        outputs["data"] = np.matmul(inputs["data"],self.params["w"].value)
        for i in range(len(outputs["data"])):
            outputs["data"][i] = np.add(outputs["data"][i],self.params["b"].value)
        
        # raise NotImplementedError("Implement This")
        return outputs

    def backward(self, output_grads):
        """The backward pass

        Arguments:
            output_grads (OrderedDict): Containing
                grad: gradient wrt output
        Returns:
            input_grads (OrderedDict): Containing
                grad: gradient wrt input

        Note that this layer also computes the gradients wrt the
        parameters (i.e you should populate the values of
        self.params["w"].grad, and self.params["b"].grad here)

        Note that you should compute the average gradient
        (i.e divide by batch_size) when you computing the gradient
        of parameters.
        """
        out = output_grads["grad"] #dl/dhi
        # raise NotImplementedError("Implement This")
        input_data = self.data
        input_grads = OrderedDict()


        batch_size = len(input_data)

        input_grads["grad"] = np.dot(out,self.params["w"].value.transpose())
        self.params["w"].grad = np.dot(out.transpose(),input_data).transpose() / dtype(batch_size)
        self.params["b"].grad = np.sum(out,axis = 0) / dtype(batch_size)

        return input_grads


class ReLULayer(object):
    """A ReLU activation layer
    """

    def __init__(self):
        self.params = OrderedDict()

    def forward(self, inputs):
        """The forward pass

        Arguments:
            inputs (``OrderedDict``): A dictionary containing
                height: the height of the current input
                width: the width of the current input
                channels: number of channels in the current
                    inputs
                data: a flattened data array n_in dimensions

        Returns:
            outputs (``OrderedDict``): A dictionary containing
                height: The height of the output (1 for a dense layer)
                width: The width of the output (1 for a dense layer)
                out_channels: The output number of feature maps
                    Same as the input channels for this layer
                data: a flattened output data array of the form
                    n_in

        Note that you only need to populate the outputs["data"]
        element.
        """
        outputs = OrderedDict()
        for key in inputs:
            if key != "data":
                outputs[key] = inputs[key]
            else:
                # hash for backward pass
                self.data = inputs[key]
                outputs["data"] = np.zeros(
                    (len(inputs["data"]), inputs["height"] * inputs["width"] * inputs["channels"]), dtype=dtype)
                outputs["data"] = np.maximum(0,inputs["data"])
                # raise NotImplementedError("Implement This")
        return outputs

    def backward(self, outputs_grad):
        """The backward pass

        Arguments:
            output_grads (OrderedDict): Containing
                grad: gradient wrt output
        Returns:
            input_grads (OrderedDict): Containing
                grad: gradient wrt input

        Note that you just compute the gradient wrt the ReLU layer
        """
        input_grads = OrderedDict()
        input_grads["grad"] = np.zeros_like(self.data,dtype = dtype)
        out = outputs_grad["grad"]
        input_grads["grad"] = np.multiply(out,np.where(np.maximum(self.data,0)<=0,0,1))
        # raise NotImplementedError("Implement This")
        return input_grads
