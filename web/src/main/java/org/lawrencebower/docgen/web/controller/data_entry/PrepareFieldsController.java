package org.lawrencebower.docgen.web.controller.data_entry;

import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.data_entry.DataEntryCB;
import org.lawrencebower.docgen.web_model.view.contact.BusinessSelection;
import org.lawrencebower.docgen.web_model.view.contact.CustomerSelection;
import org.lawrencebower.docgen.web_model.view.document.DocumentSet;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_model.view.product.ProductSelection;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Scope("session")
public class PrepareFieldsController {

    private DataEntryCB business;
    private SessionData sessionData;

    @Autowired
    public void setBusiness(DataEntryCB business) {
        this.business = business;
    }

    @Autowired
    public void setSessionData(SessionData sessionData) {
        this.sessionData = sessionData;
    }

    public void prepareFields() {

        setRelevantDocuments();

        mapAutoMappedFields();

        injectProductFields();

        processCalculatedFields();
    }

    private void setRelevantDocuments() {

        CustomerSelection customerSelection = sessionData.getCustomerSelection();
        ProductSelection productSelection = sessionData.getProductSelection();

        DocumentSet documentsForViewing =
                business.getDocumentsForViewing(customerSelection, productSelection);

        sessionData.setDocuments(documentsForViewing);
    }

    private void mapAutoMappedFields() {

        DocumentSet documentSet = sessionData.getDocuments();
        CustomerSelection customerSelection = sessionData.getCustomerSelection();
        BusinessSelection businessSelection = sessionData.getBusinessSelection();

        business.mapAutoMapComponents(documentSet,
                                      customerSelection,
                                      businessSelection);
    }

    private void injectProductFields() {

        DocumentSet documentSet = sessionData.getDocuments();
        List<ProductView> selectedProducts = sessionData.getSelectedProducts();

        business.injectProductFields(documentSet, selectedProducts);
    }

    private void processCalculatedFields() {
        DocumentSet documentSet = sessionData.getDocuments();
        business.processCalculatedFields(documentSet);
    }

    public List<DocComponentView> getDocComponentViews() {

        DocumentSet documentSet = sessionData.getDocuments();

        boolean showAutoMappedFields = sessionData.isShowAutoMappedFields();

        return business.getComponentsForViewing(documentSet, showAutoMappedFields);
    }

}
