require! fs 
require! csv

idcount = 0
csv!from.path __dirname+'/data 2/articles.csv' { delimiter: ',', escape: '"' }
#csv!from.path __dirname+'/test.txt' { delimiter: ',' , escape: '"'}
.to.stream fs.createWriteStream __dirname+'/simple.out'
.transform (row) ->
 if row.length != 7 
    return []
# 1,htt,title,summary,"",2012-04-08 16:50:03,t,t,[],[],"",f,[],[]

 myRow = Array!
 myRow[0] = idcount++
 myRow[1] = row[2]  #http
 myRow[2] = row[1] #title
 myRow[3] = "''"
 myRow[4] = "''"
 myRow[5] = row[4]  #timestamp
 myRow[6] = "f"
 myRow[7] = "f"
 myRow[8] = '[]'
 myRow[9] = '[]'
 myRow[10] =  "'" + row[3] + "'" #provider
 myRow[11] = "f"
 myRow[12] = '[]'
 myRow[13] = '[]'
 myRow
#.on 'record' !(row, index) -> console.log '#'+index+' '+JSON.stringify row 
.on 'close' !(count) -> console.log 'Number of lines: '+count
.on 'error' !(error) -> console.log error.message