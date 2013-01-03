package org.lawrencebower.docgen.web_model.view.document;

public abstract class DocumentViewFactory {

//    public DocumentView createDocumentView(Document doc){
//        DocumentView documentView = createDocumentView();
//        documentView.setDocument(doc);
//        return documentView;
//    }

    public abstract DocumentViewImpl createDocumentView();

}
