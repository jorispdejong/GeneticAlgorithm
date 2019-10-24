using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;
using UnityEngine.UI;

public class TennisGame : MonoBehaviour
{
    public GameObject tennisBall;
    // public GameObject tennisBallInstance;
    private List<Dictionary<string,object>> data;
    private int current = 0;

    private GUIStyle guiStyle = new GUIStyle();

    private string dataFileName = "test_data";

    private CoordsTime coordsTime;
    private GameObject[] balls;

    // Start is called before the first frame update
    void Start()
    {
        // ReadCSV(@"/Users/dunnkers/git/GeneticAlgorithm/test_data.txt");
        // tennisBallInstance = Instantiate(
        //     tennisBall, new Vector3(0, 0, 0), Quaternion.identity
        // );

        // read in "csv" data
        // data = CSVReader.Read("test_data");
        // print("CSV file read.");

        LoadData();
    }

    private void LoadData()
    {

        TextAsset data = Resources.Load (dataFileName) as TextAsset;
        coordsTime = JsonUtility.FromJson<CoordsTime>(data.text);

        // instantiate balls
        int ballCount = coordsTime.coords_time[0].balls.Length;
        balls = new GameObject[ballCount];
        for (int i = 0; i < ballCount; i++)
        {
            balls[0] = Instantiate(
                tennisBall, new Vector3(0, 0, 0), Quaternion.identity
            );
        }

        print(balls.Length);
        // // Path.Combine combines strings into a file path
        // // Application.StreamingAssets points to Assets/StreamingAssets in the Editor, and the StreamingAssets folder in a build
        // string filePath = Path.Combine(Application.streamingAssetsPath, dataFileName);

        // if(File.Exists(filePath))
        // {
        //     // Read the json from the file into a string
        //     string dataAsJson = File.ReadAllText(filePath);    
        //     // Pass the json to JsonUtility, and tell it to create a GameData object from it
        //     GameData loadedData = JsonUtility.FromJson<GameData>(dataAsJson);

        //     // Retrieve the allRoundData property of loadedData
        //     allRoundData = loadedData.allRoundData;
        // }
        // else
        // {
        //     Debug.LogError("Cannot load game data!");
        // }
    }


    void OnGUI()
    {
        if (current >= coordsTime.coords_time.Length) { // reset when all positions looped.
            current = 0;
        }
        
        int px = 30;
        guiStyle.fontSize = px;

        GUI.Label(new Rect(0, px * 0, 100, px), 
            "FPS: " + (int)(1.0f / Time.smoothDeltaTime), guiStyle);

        GUI.Label(new Rect(0, px * 1, 100, px), 
            "Generation: " + 
            coordsTime.coords_time[current].generation, guiStyle);
    }

    // Update is called once per frame
    void Update()
    {
        if (current >= coordsTime.coords_time.Length) { // reset when all positions looped.
            current = 0;
        }
        
        // transform.position += Vector3.forward * Time.deltaTime;
        // float z = Convert.ToSingle(data[current]["V3"]);
        // float x = Convert.ToSingle(data[current]["V4"]);
        // float y = Convert.ToSingle(data[current]["V5"]);
        // float z = (float) coordsTime.coords_time[current].balls[0].x;
        // float x = (float) coordsTime.coords_time[current].balls[0].y;
        // float y = (float) coordsTime.coords_time[current].balls[0].z;
        // tennisBallInstance.transform.position = new Vector3(x, y, z);

        CoordTime coordTime = coordsTime.coords_time[current];
        for (int i = 0; i < coordTime.balls.Length; i++)
        {
            ObjectCoords ball = coordTime.balls[i];
            float z = (float) ball.x;
            float x = (float) ball.y;
            float y = (float) ball.z;
            balls[i].transform.position = new Vector3(x, y, z);
        }

        int fps = (int) (1.0f / Time.smoothDeltaTime);
        if (fps < 0) {
            current += 1;
        } else {
            current += fps;
        }
    }

    // void ReadCSV(string filepath)
    // {
    //     using(var reader = new StreamReader(filepath))
    //     {
    //         List<string> listA = new List<string>();
    //         List<string> listB = new List<string>();
    //         bool head = false;
    //         while (!reader.EndOfStream)
    //         {
    //             var line = reader.ReadLine();
    //             var values = line.Split('\t');

    //             if (!head) {
    //                 head = true;
    //                 continue;
    //             }

    //             listA.Add(values[0]);
    //             listB.Add(values[1]);
    //         }
    //     }

    // }
}
