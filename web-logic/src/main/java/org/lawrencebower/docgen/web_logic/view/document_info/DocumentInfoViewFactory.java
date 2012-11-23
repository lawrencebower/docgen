package org.lawrencebower.docgen.web_logic.view.document_info;

import org.lawrencebower.docgen.core.document.DocumentInfo;

public abstract class DocumentInfoViewFactory {

    public DocumentInfoView createDocumentInfoView(DocumentInfo docInfo){
        DocumentInfoView infoView = createDocumentInfoView();
        infoView.setDocumentInfo(docInfo);
        return infoView;
    }

    public abstract DocumentInfoView createDocumentInfoView();

}
