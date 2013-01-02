package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.vendor;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAMComponent;
import org.lawrencebower.docgen.web_logic.view.document.component.AMComponentInfo;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AMVendorCountry extends AbstractAMComponent {

    public AMVendorCountry() {
        name = AutoMappedField.VENDOR_COUNTRY;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AMComponentInfo info) {

        String value = info.getVendorCountry();
        setComponentValueIfMatch(docComponentView, value);
    }
}
