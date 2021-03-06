<%@ taglib prefix="sf" uri="http://www.springframework.org/tags/form" %>
<%@ include file="/WEB-INF/jsp/include.jsp" %>

<jsp:useBean id="table"
             scope="request"
             type="org.lawrencebower.docgen.web_model.view.document.component.TableComponentView"/>

<c:set var="fieldSeperator" value="~" scope="application"/>

<input name="components[${fieldIndex}].tableData.tableName"
       value="${table.name}"
       id="${table.name}"
       type="hidden"/>

<table cellpadding="0" border="1">
    <tr>
        <c:forEach var="headerCell"
                   items="${table.headerCells}">
            <td>
                <c:out value="${headerCell.component.textString}"/>
            </td>
        </c:forEach>
    </tr>
    <c:forEach var="row"
               items="${table.tableRows}"
               varStatus="rowIndex">

        <input name="components[${fieldIndex}].tableData.rows[${rowIndex.index}].rowName"
               value="${row.rowName}"
               id="${row.rowName}"
               type="hidden"/>

        <tr>
            <c:forEach var="cell"
                       items="${row.cells}"
                       varStatus="cellIndex">
                <td>
                    <input name="components[${fieldIndex}].tableData.rows[${rowIndex.index}].cells[${cellIndex.index}].name"
                           value="${table.headerCells[cellIndex.index].name}"
                           id="${fieldName}"
                           type="hidden"/>

                    <c:if test="${!cell.textArea}">
                        <input name="components[${fieldIndex}].tableData.rows[${rowIndex.index}].cells[${cellIndex.index}].value"
                               value="${cell.component.textString}"
                               id="${fieldName}"/>
                    </c:if>

                    <c:if test="${cell.textArea}">
                        <textarea
                                name="components[${fieldIndex}].tableData.rows[${rowIndex.index}].cells[${cellIndex.index}].value"
                                id="${fieldName}" rows="3" cols="30"><c:out
                                value="${cell.component.textString}"/></textarea>
                    </c:if>

                </td>
            </c:forEach>
        </tr>
    </c:forEach>
</table>
