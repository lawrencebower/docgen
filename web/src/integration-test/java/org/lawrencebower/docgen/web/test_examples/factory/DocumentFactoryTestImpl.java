package org.lawrencebower.docgen.web.test_examples.factory;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web.test_examples.test_doc_1.TestDocument1;
import org.lawrencebower.docgen.web.test_examples.test_doc_2.TestDocument2;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewImpl;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.DocumentFactory;

import java.util.Arrays;
import java.util.List;

public abstract class DocumentFactoryTestImpl implements DocumentFactory {

    public static final String DOC_1_NAME = TestDocument1.TEST_DOC_1_NAME;
    public static final String DOC_2_NAME = TestDocument2.TEST_DOC_2_NAME;
    public static final String AUTO_MAPPED_EXAMPLE_FIELD = AutoMappedField.VENDOR_ADDRESS.getName();
    public static final String EXAMPLE_FIELD = "Date:";
    public static final String PRODUCT_MODEL_1 = "W1";
    public static final String PRODUCT_MODEL_2 = "W2";
    public static final String PRODUCT_MODEL_3 = "W3";
    public static final String PRODUCT_MODEL_4 = "";//product 4 has no model number
    public static final String CUSTOMER_ID_1 = "1";
    public static final String CUSTOMER_ID_2 = "2";

    @Override
    public List<DocumentView> getAllDocuments() {

        TestDocument1 testDocument1 = getDeliveryNote();
        DocumentView deliveryNoteView = testDocument1.getDocumentView();

        TestDocument2 testDocument2 = getFCC740();
        DocumentViewImpl fcc740View = testDocument2.getDocumentView();

        return Arrays.asList(deliveryNoteView,
                             fcc740View);
    }

    @Override
    public DocumentViewImpl createDocument(String documentName) {

        DocumentViewImpl result;

        switch (documentName) {
            case TestDocument1.TEST_DOC_1_NAME:
                TestDocument1 testDocument1 = getDeliveryNote();
                result = testDocument1.getDocumentView();
                break;
            case TestDocument2.TEST_DOC_2_NAME:
                TestDocument2 testDocument2 = getFCC740();
                result = testDocument2.getDocumentView();
                break;
            default:
                String message = String.format("No mapping present for documentName '%s'", documentName);
                throw new DocGenException(message);
        }

        return result;
    }

    protected abstract TestDocument1 getDeliveryNote();

    protected abstract TestDocument2 getFCC740();
}
