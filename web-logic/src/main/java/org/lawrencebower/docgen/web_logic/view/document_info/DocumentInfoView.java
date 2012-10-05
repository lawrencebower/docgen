package org.lawrencebower.docgen.web_logic.view.document_info;

import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.exception.DocGenException;

public class DocumentInfoView {

    private DocumentInfo documentInfo;

    private boolean isDirty = false;
    private boolean isComplete = false;

    public DocumentInfoView(DocumentInfo documentInfo) {
        if(documentInfo == null){
            throw new DocGenException("DocumentInfo parameter is null");
        }
        this.documentInfo = documentInfo;
    }

    public boolean isDirty() {
        return isDirty;
    }

    public boolean isComplete() {
        return isComplete;
    }

    public String getName(){
        return documentInfo.getName();
    }

    public DocumentInfo getDocument() {
        return documentInfo;
    }
}
