package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class DataSet {

	private List<DataRow> dataRows;
	
	public DataSet() {
        dataRows = new ArrayList<>();
	}
	
	public void addRow(DataRow dataRow) {
        dataRows.add(dataRow);
	}

	public List<DataRow> getRows() {
		return Collections.unmodifiableList(dataRows);
	}
	
	public boolean contains(int columnIndex, Object value) {
		return findRow(columnIndex, value) != null;
	}

	public DataRow findRow(int columnIndex, Object value) {

        DataRow dataRow = null;

		for (DataRow row : dataRows) {
			Object columnValue = row.getColumn(columnIndex);
			if (value.equals(columnValue)) {
				dataRow = row;
			}
		}

		return dataRow;
	}
	
}
