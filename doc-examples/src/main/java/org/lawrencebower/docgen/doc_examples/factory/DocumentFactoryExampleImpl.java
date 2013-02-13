package org.lawrencebower.docgen.doc_examples.factory;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.doc_examples.commercial_invoice.CommercialInvoice;
import org.lawrencebower.docgen.doc_examples.delivery_note.DeliveryNote;
import org.lawrencebower.docgen.doc_examples.fcc_740.FCC_740;
import org.lawrencebower.docgen.doc_examples.fda_2877.FDA_2877;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewImpl;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.DocumentFactory;

import java.util.Arrays;
import java.util.List;

public abstract class DocumentFactoryExampleImpl implements DocumentFactory {

    @Override
    public List<DocumentView> getAllDocuments() {

        CommercialInvoice commercialInvoice = getCommercialInvoice();
        DocumentView commercialInvoiceView = commercialInvoice.getDocumentView();

        DeliveryNote deliveryNote = getDeliveryNote();
        DocumentView deliveryNoteView = deliveryNote.getDocumentView();

        FDA_2877 fda2877 = getFDA2887();
        DocumentView fda2887View = fda2877.getDocumentView();

        FCC_740 fcc740 = getFCC740();
        DocumentViewImpl fcc740View = fcc740.getDocumentView();

        return Arrays.asList(commercialInvoiceView,
                             deliveryNoteView,
                             fda2887View,
                             fcc740View);
    }

    @Override
    public DocumentViewImpl createDocument(String documentName) {

        DocumentViewImpl result;

        switch (documentName) {
            case CommercialInvoice.INVOICE_NAME:
                CommercialInvoice commercialInvoice = getCommercialInvoice();
                result = commercialInvoice.getDocumentView();
                break;
            case DeliveryNote.DELIVERY_NOTE_NAME:
                DeliveryNote deliveryNote = getDeliveryNote();
                result = deliveryNote.getDocumentView();
                break;
            case FDA_2877.FDA_2877_NAME:
                FDA_2877 fda_2877 = getFDA2887();
                result = fda_2877.getDocumentView();
                break;
            case FCC_740.FCC_740_NAME:
                FCC_740 fcc_740 = getFCC740();
                result = fcc_740.getDocumentView();
                break;
            default:
                String message = String.format("No mapping present for documentName '%s'", documentName);
                throw new DocGenException(message);
        }

        return result;
    }

    protected abstract CommercialInvoice getCommercialInvoice();

    protected abstract DeliveryNote getDeliveryNote();

    protected abstract FDA_2877 getFDA2887();

    protected abstract FCC_740 getFCC740();
}