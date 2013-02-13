package org.lawrencebower.docgen.web_model.view.contact;

import org.lawrencebower.docgen.core.exception.DocGenException;

public class BusinessSelection {

    public static final String NO_BUSINESS_SELECTED = "No business selected?!";

    private ContactView selectedBusiness;

    public void selectBusiness(ContactView contact) {
        selectedBusiness = contact;
    }

    public ContactView getSelectedBusiness() {
        return selectedBusiness;
    }

    public void checkBusinessSet() {
        if (selectedBusiness == null) {
            throw new DocGenException(NO_BUSINESS_SELECTED);
        }
    }

    public void clear() {
        selectedBusiness = null;
    }
}
