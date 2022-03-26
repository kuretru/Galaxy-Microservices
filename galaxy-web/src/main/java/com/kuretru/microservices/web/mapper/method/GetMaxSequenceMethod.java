package com.kuretru.microservices.web.mapper.method;

import com.baomidou.mybatisplus.core.injector.AbstractMethod;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import org.apache.ibatis.mapping.MappedStatement;
import org.apache.ibatis.mapping.SqlSource;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class GetMaxSequenceMethod extends AbstractMethod {

    public GetMaxSequenceMethod(String name) {
        super(name);
    }

    @Override
    public MappedStatement injectMappedStatement(Class<?> mapperClass, Class<?> modelClass, TableInfo tableInfo) {
        String sqlTemplate = "<script>\n%s SELECT MAX(sequence) FROM %s %s %s\n</script>";
        String tableName = tableInfo.getTableName();
        String where = super.sqlWhereEntityWrapper(true, tableInfo);
        String sql = String.format(sqlTemplate, sqlFirst(), tableName, where, sqlComment());
        SqlSource sqlSource = languageDriver.createSqlSource(configuration, sql, modelClass);
        return this.addSelectMappedStatementForOther(mapperClass, this.methodName, sqlSource, Integer.class);
    }

}
