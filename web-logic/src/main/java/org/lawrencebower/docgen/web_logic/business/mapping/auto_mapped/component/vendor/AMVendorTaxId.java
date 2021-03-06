package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.vendor;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAMComponent;
import org.lawrencebower.docgen.web_model.business_def.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;

public class AMVendorTaxId extends AbstractAMComponent {

    public AMVendorTaxId() {
        name = AutoMappedField.VENDOR_TAX_ID;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AMComponentInfo info) {

        String value = info.getVendorTaxId();
        setComponentValueIfMatch(docComponentView, value);
    }
}
