package com.kuretru.api.common.mapper.method;

import com.baomidou.mybatisplus.core.injector.AbstractMethod;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import org.apache.ibatis.mapping.MappedStatement;
import org.apache.ibatis.mapping.SqlSource;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class UpdateSequenceByUuidsMethod extends AbstractMethod {

    public UpdateSequenceByUuidsMethod(String name) {
        super(name);
    }

    @Override
    public MappedStatement injectMappedStatement(Class<?> mapperClass, Class<?> modelClass, TableInfo tableInfo) {
        String sqlTemplate = "<script>\n%s UPDATE %s SET sequence = CASE %s END\n</script>";
        String tableName = tableInfo.getTableName();
        String sql = "";
        SqlSource sqlSource = languageDriver.createSqlSource(configuration, sql, modelClass);
        return this.addUpdateMappedStatement(mapperClass, modelClass, this.methodName, sqlSource);
    }

}
