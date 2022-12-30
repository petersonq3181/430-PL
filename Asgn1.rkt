#lang scribble/manual

Dear Upper Management at MicroGoogleBook,

We will be building a self-driving car. This essay has two main parts:
a) criteria for a programming language to be suitable for the task
and b) a suggested programming language.

Part A.

A self-driving car requires many components.
Namely, it likely requires some deep neural networks to perform
the computer vision and object detection which serves as the car's
"sight". It also will require some type of interface to the car's
mechanical controls.

Based on the neural network requirement, the programming language
will need to have robust machine learning libraries. Furthermore,
the language needs to be relatively fast to ensure that training
the neural networks completes in reasonable time, and does not
take years to complete. Also, because deep neural networks are
essentially layers of nodes, which can be represented best in
matrices, it would be suitable for the language to be able to
efficiently handle matrices and matrix operations. 

Based on the control interface requirement, it would be helpful for
the programming language to be fairly specific. For example, one
would not want the programming language to accidentally confuse control
of the gas pedal with control of the steering wheel. To handle the car's
controls, it would be good for the programming language to have classes
or some well-defined form of object oriented programming (OOP),
and it would also be best to be statically typed rather than
dynamically typed, to make the code more explicit and clear. 

Part B.

Given the criteria, I suggest using Matlab. As a back-up, I
suggest using Python.

First, Matlab is suitable for the first main criteria: machine learning.
Matlab, which stands for "matrix laboratory" is built mainly for matrix
operations. Also, Matlab has good machine learning and computer vision
packages and documentation. Beyond its many machine learning libraries,
which include things such as Convolutional Neural Networks, Matlab also
has an automated driving toolbox (https://www.mathworks.com/products/
automated-driving.html).
This toolbox can directly help with the project's goals.

Matlab does not perfectly fulfill the second main requirement.
Matlab is dynamically typed, so it is not as specific as a
statically typed language. Also, Matlab was not designed for OOP, and thus
suffers in that aspect. Matlab still does support OOP and does have
classes, but because this is not its main purpose, and because people
generally tend towards other languages such as Java or C++ for OOP,
it is not perfect. Nonetheless, despite being dynamically typed and having
sub-par OOP, Matlab can still satisfy the control interface requirements
with the car's controls.

Here is why I suggest Python as a back-up: it fulfills some of
the first requirement, namely by having good machine learning
libraries, and it does much better than Matlab at fulfilling the
second requirement, namely by having good OOP. Again, however,
Python is not particularly efficient with large matrix computations, which
might make it in-efficient in both the training and implementation of the
deep neural networks. Python is also not statically typed, which might
lend to a bit less control or explicitness in the code.

Conclusion. 

Both Matlab and Python have their strengths and weaknesses in
fulfilling the project's requirements. I would contend that no
existing programming language is perfect for all the requirements
of the project, so the weaknesses of these languages are acceptable.
Try Matlab first, and as a back-up resort to Python.  

Best,
[A rising ⭒ at MicroGoogleBook]
