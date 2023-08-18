# statistical_learning_protein_contacts
## Classification of Contact Types In Protein Structures
In this Project, different models (LDA, KNN, Logistic Classifier, Naive Bayes, Multinomial Model) were employed to perform the task of classification, the results were evaluated with different metric scores and discussed.

Proteins are biological entities that are composed of amino acids/residues.

There are 20 amino acids encountered in the nature, differing from each other with different properties, such as charge, size, polarity etc. The difference among the proteins is due to the difference of amino acids of which they are comprised.

Right after being synthesized, a protein must be folded to a specific structure in 3D space in order to have functionality. This folding process takes place via building chemical interactions among its amino acids. Different amino acids have different interactions with different strengths between them because of their physical properties. 

These interaction types can be categorized as:
* Hydrogen Bonds (HBOND)
* Van Der Waals Interactions (VDW)
* Disulfide Bridges (SBOND)
* Salt Bridges (IONIC)
* 𝜋-𝜋 Stacking (PIPISTACK)
* 𝜋-Cation (PICATION)

**The aim** of this project is to predict the contact types, given different features of two amino acids that are in contact.
To achieve this purpose, a predictive model was needed to calculate the probabilities of different contact types, which the residue pair (amino acid pair) might have. The model must evaluate the features of amino acids in contact and assign a type of interaction to each contact, based on the experience obtained from the given training set. Considering all of these, it is obvious that the problem handled in this project is a multi-class classification problem.
