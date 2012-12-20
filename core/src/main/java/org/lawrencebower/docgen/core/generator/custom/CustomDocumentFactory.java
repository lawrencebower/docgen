package org.lawrencebower.docgen.core.generator.custom;

public abstract class CustomDocumentFactory {

    public CustomDocument getCustomDocument(String docName){
        CustomDocument customDocument = getCustomDocumentBean();
        customDocument.setName(docName);
        return customDocument;
    }

    protected abstract CustomDocument getCustomDocumentBean();
}
