package org.lawrencebower.docgen.web_logic.view.view_factory;

import org.lawrencebower.docgen.doc_examples.commercial_invoice.CommercialInvoice;
import org.lawrencebower.docgen.doc_examples.delivery_note.DeliveryNote;
import org.lawrencebower.docgen.doc_examples.factory.ProductFactoryTestImpl;
import org.lawrencebower.docgen.doc_examples.fcc_740.FCC_740;
import org.lawrencebower.docgen.doc_examples.fda_2887.FDA_2887;

import static org.lawrencebower.docgen.doc_examples.factory.CustomerFactoryTestImpl.CUSTOMER_ID_1;
import static org.lawrencebower.docgen.doc_examples.factory.CustomerFactoryTestImpl.CUSTOMER_ID_2;

class CustomerProductMappingFactoryTestImpl implements CustomerProductMappingFactory {

    private CustomerProduct_Document_Mappings customerProductDocMappings = new CustomerProduct_Document_Mappings();

    @Override
    public CustomerProduct_Document_Mappings getMappingInfo() {
        return customerProductDocMappings;
    }

    private void initMappings() {
        initContact1();
        initContact2();
    }

    private void initContact2() {
        addMappingInfo(CUSTOMER_ID_2,
                       ProductFactoryTestImpl.PRODUCT_ID_1,
                       CommercialInvoice.INVOICE_NAME);

        addMappingInfo(CUSTOMER_ID_2,
                       ProductFactoryTestImpl.PRODUCT_ID_1,
                       DeliveryNote.DELIVERY_NOTE_NAME);

        addMappingInfo(CUSTOMER_ID_2,
                       ProductFactoryTestImpl.PRODUCT_ID_2,
                       CommercialInvoice.INVOICE_NAME);

        addMappingInfo(CUSTOMER_ID_2,
                       ProductFactoryTestImpl.PRODUCT_ID_1,
                       DeliveryNote.DELIVERY_NOTE_NAME);

        addMappingInfo(CUSTOMER_ID_2,
                       ProductFactoryTestImpl.PRODUCT_ID_1,
                       FDA_2887.FDA_2887_NAME);

        addMappingInfo(CUSTOMER_ID_2,
                       ProductFactoryTestImpl.PRODUCT_ID_1,
                       FCC_740.FCC_740_NAME);
    }

    private void initContact1() {
        addMappingInfo(CUSTOMER_ID_1,
                       ProductFactoryTestImpl.PRODUCT_ID_1,
                       CommercialInvoice.INVOICE_NAME);

        addMappingInfo(CUSTOMER_ID_1,
                       ProductFactoryTestImpl.PRODUCT_ID_1,
                       DeliveryNote.DELIVERY_NOTE_NAME);

        addMappingInfo(CUSTOMER_ID_1,
                       ProductFactoryTestImpl.PRODUCT_ID_2,
                       CommercialInvoice.INVOICE_NAME);

        addMappingInfo(CUSTOMER_ID_1,
                       ProductFactoryTestImpl.PRODUCT_ID_2,
                       DeliveryNote.DELIVERY_NOTE_NAME);
    }

    private void addMappingInfo(String customerId,
                                String productId,
                                String documentName) {

        customerProductDocMappings.addDocument(customerId, productId, documentName);
    }
}
