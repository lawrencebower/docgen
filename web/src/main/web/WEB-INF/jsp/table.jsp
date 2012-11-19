<%@ taglib prefix="sf" uri="http://www.springframework.org/tags/form" %>
<%@ include file="/WEB-INF/jsp/include.jsp" %>

<jsp:useBean id="table" scope="request"
             type="org.lawrencebower.docgen.web_logic.view.document_info.component.TableComponentView"/>
<c:set var="fieldSeperator" value="~" scope="application"/>

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
        <tr>
            <c:forEach var="cell"
                       items="${row.cells}"
                       varStatus="cellIndex">
                <td>
                    <c:set var="fieldName"
                           value="${table.name}${fieldSeperator}${rowIndex.count}${fieldSeperator}${cellIndex.count}"/>
                    <input name="${fieldName}"
                           value="${cell.component.textString}"
                           id="${fieldName}"/>
                </td>
            </c:forEach>
        </tr>
    </c:forEach>
</table>
