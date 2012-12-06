package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

public class AutoMappedVendorContactName extends AutoMappedComponent {

    public AutoMappedVendorContactName() {
        name = VENDOR_CONTACT_NAME;
    }

    @Override
    public void mapComponent(DocComponentView docComponentView,
                             AutoMappedComponentInfo info) {

        String value = info.getVendorContactName();
        setComponentValueIfMatch(docComponentView, value);
    }
}
