package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.StringUtils;

public class DataRow {

	public static final String ARRAY_SEPARATOR = ",";
	
	private Object[] values;

    /**
     * used in the tsv files to represent comma characters - are replaced upon parsing
     */
    private static final String COMMA_TOKEN = "{COM}";

    public DataRow() {
        values = new Object[0];
	}
	
	public DataRow(Object... values) {
		this.values = values;
	}
	
	public DataRow(String... values) {
		this.values = values;
	}
	
	public boolean hasColumn(int columnIndex) {
		return values.length >= (columnIndex + 1);
	}

	public Object getColumn(int columnIndex) {
		return values[columnIndex];
	}

	public String getString(int columnIndex) {
		return (String) getColumn(columnIndex);
	}

	public String getStringDefault(int columnIndex, String defValue) {

		if (columnIndex < values.length) {
			return (String) getColumn(columnIndex);
		} else {
			return defValue;
		}
	}
	
	public String[] getStringArray(int columnIndex) {
		String value = getString(columnIndex);
		String[] result = StringUtils.split(value, ARRAY_SEPARATOR);
        for(int i = 0; i<result.length;i++){
            result[i] = result[i].replace(COMMA_TOKEN,",");
        }

		return result;
	}

	@Override
	public String toString() {
		return ArrayUtils.toString(values);
	}

    public int getLength(){
        return values.length;
    }
	
}
