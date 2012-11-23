package org.lawrencebower.docgen.web_logic.view.document_info;

public abstract class DocumentInfoSetFactory {

    public DocumentInfoSet createDocumentInfoSet(DocumentInfoView... docInfoViews){
        DocumentInfoSet infoSet = createDocumentInfoSet();
        infoSet.setDocuments(docInfoViews);
        return infoSet;
    }

    public abstract DocumentInfoSet createDocumentInfoSet();

}
