package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.vendor;

import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedFields;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AMComponentInfo;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.component.AbstractAMComponent;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AMVendorTaxId extends AbstractAMComponent {

    public AMVendorTaxId() {
        name = AutoMappedFields.VENDOR_TAX_ID;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AMComponentInfo info) {

        String value = info.getVendorTaxId();
        setComponentValueIfMatch(docComponentView, value);
    }
}
