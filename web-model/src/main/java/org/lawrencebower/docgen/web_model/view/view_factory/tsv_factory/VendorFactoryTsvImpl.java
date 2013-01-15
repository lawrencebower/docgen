package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.view_factory.factory.VendorFactory;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.DataRow;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.DataSet;
import org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser.TSVReader;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

public class VendorFactoryTsvImpl implements VendorFactory {

    private String vendorTSVFile;

    @Autowired
    private TSVReader tsvReader;
    @Autowired
    private ContactMapper contactMapper;

    private ContactView vendor;

    @Override
    public ContactView getVendor() {
        return vendor;
    }

    public void setVendorTSVFile(String vendorTSVFile) {
        this.vendorTSVFile = vendorTSVFile;
    }

    public void initVendor() {
        DataSet dataSet = tsvReader.readDataSetAsFile(vendorTSVFile);
        List<DataRow> rows = dataSet.getRows();

        checkRowCount(rows);

        DataRow dataRow = rows.get(0);

        vendor = contactMapper.mapCustomerInfo(dataRow);
    }

    private void checkRowCount(List<DataRow> rows) {
        if(rows.size() != 1){
            String message = String.format("more than/less than 1 vendor defined in '%s'?!", vendorTSVFile);
            throw new DocGenException(message);
        }
    }
}
