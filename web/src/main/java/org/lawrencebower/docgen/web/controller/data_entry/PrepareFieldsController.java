package org.lawrencebower.docgen.web.controller.data_entry;

import org.apache.log4j.Logger;
import org.lawrencebower.docgen.web.model.SessionData;
import org.lawrencebower.docgen.web_logic.business.controler_business.data_entry.DataEntryCB;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document.DocumentSet;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.util.List;

@Controller
@Scope("session")
public class PrepareFieldsController {

    static Logger logger = Logger.getLogger(PrepareFieldsController.class);

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

    @RequestMapping(value = "/dataEntry/prepareFields", method = RequestMethod.GET)
    public String prepareFields() {

        setRelevantDocuments();

        mapAutoMappedFields();

        injectProductFields();

        processCalculatedFields();

        return "dataEntry";
    }

    private void setRelevantDocuments() {

        ContactView selectedCustomer = sessionData.getSelectedCustomer();
        List<ProductView> selectedProducts = sessionData.getSelectedProducts();

        DocumentSet documentsForViewing =
                business.getDocumentsForViewing(selectedCustomer, selectedProducts);

        sessionData.setDocuments(documentsForViewing);
    }

    private void mapAutoMappedFields() {

        ContactView selectedBusiness = sessionData.getSelectedBusiness();
        ContactView selectedCustomer = sessionData.getSelectedCustomer();
        DocumentSet documentSet = sessionData.getDocuments();

        business.mapAutoMapComponents(documentSet,
                                      selectedCustomer,
                                      selectedBusiness);
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
