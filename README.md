# kleene

Generate a Kleene tree.

### Files

* `kleene.py`: Enumerates the branches which should be cut from the complete
  binary tree in order to be left with a Kleene tree.
* `plotcuts.py`: Generates a (progressive) plot of the cuts.
* `plottree.py`: Generates a (progressive) plot of the Kleene tree.

### Requirements

Plotting uses `matplotlib`, (which depends on `tkinter`).

Install using
```
pip3 install matplotlib --user
```

or
```
python3 -m pip install matplotlib --user
```


### Results

![Screenshot](https://i.imgur.com/5LGVtxS.png "First 20 levels of the tree")
