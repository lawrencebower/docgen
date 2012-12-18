package org.lawrencebower.docgen.core.generator.custom;

public abstract class CustomDocumentFactory {

    public CustomDocument getCustomDocument(String docName){
        CustomDocument customDocument = getCustomDocument();
        customDocument.setName(docName);
        return customDocument;
    }

    abstract CustomDocument getCustomDocument();

}
