package org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped;

import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

import java.util.ArrayList;
import java.util.List;

public class AutoMappedComponentMapper {

    private List<AutoMappedComponent> componentMappers = new ArrayList<>();

    public AutoMappedComponentMapper() {
        initMappers();
    }

    private void initMappers() {

        componentMappers.add(new AutoMappedBusinessName());
        componentMappers.add(new AutoMappedBusinessNameAndAddress());
        componentMappers.add(new AutoMappedBusinessAddress());
        componentMappers.add(new AutoMappedBusinessContactName());
        componentMappers.add(new AutoMappedBusinessCountry());
        componentMappers.add(new AutoMappedBusinessPhone());

        componentMappers.add(new AutoMappedCustomerName());
        componentMappers.add(new AutoMappedCustomerNameAndAddress());
        componentMappers.add(new AutoMappedCustomerAddress());
        componentMappers.add(new AutoMappedCustomerContactName());
        componentMappers.add(new AutoMappedCustomerCountry());
        componentMappers.add(new AutoMappedCustomerPhone());

        componentMappers.add(new AutoMappedVendorName());
        componentMappers.add(new AutoMappedVendorNameAndAddress());
        componentMappers.add(new AutoMappedVendorAddress());
        componentMappers.add(new AutoMappedVendorContactName());
        componentMappers.add(new AutoMappedVendorCountry());
        componentMappers.add(new AutoMappedVendorPhone());
        componentMappers.add(new AutoMappedVendorEmail());
        componentMappers.add(new AutoMappedVendorTaxId());
    }

    public void mapComponent(DocComponentView componentView, AutoMappedComponentInfo mappingInfo) {
        for (AutoMappedComponent componentMapper : componentMappers) {
            /**
             * todo
             * will continue through all mappers, even once matched, this is because I intend to refactor
             * this class type to use String replacement, which will run over all mappers
             */
            componentMapper.mapComponent(componentView, mappingInfo);
        }
    }

    public boolean matchesName(String componentName) {

        boolean nameMatched = false;

        for (AutoMappedComponent componentMapper : componentMappers) {
            if (componentMapper.matchesName(componentName)) {
                nameMatched = true;
                break;
            }
        }

        return nameMatched;
    }
}
